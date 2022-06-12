(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* From lambda to assembly code *)

[@@@ocaml.warning "+a-4-9-40-41-42-44"]

open Format
open Config
open Clflags
open Misc
open Cmm

type error =
  | Assembler_error of string
  | Mismatched_for_pack of string option
  | Asm_generation of string * Emitaux.error

exception Error of error

let cmm_invariants ppf fd_cmm =
  let print_fundecl =
    if !Clflags.dump_cmm then Printcmm.fundecl
    else fun ppf fdecl -> Format.fprintf ppf "%s" fdecl.fun_name
  in
  if !Clflags.cmm_invariants && Cmm_invariants.run ppf fd_cmm then
    Misc.fatal_errorf "Cmm invariants failed on following fundecl:@.%a@."
      print_fundecl fd_cmm;
  fd_cmm

let liveness platform phrase = Liveness.fundecl platform phrase; phrase

let dump_if
    (type a s)
    (module Printlinear : Printlinear.S
        with type addressing_mode = a
         and type specific_operation = s
    )
    ppf
    flag
    message
    phrase
  =
  if !flag then Printlinear.Printmach.phase message ppf phrase

let pass_dump_if print_phase ppf flag message phrase =
  dump_if print_phase ppf flag message phrase; phrase

let pass_dump_linear_if
    (type a s)
    (module Printlinear
      : Printlinear.S
        with type addressing_mode = a
         and type specific_operation = s
    )
    ppf
    flag
    message
    phrase
  =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let start_from_emit = ref true

type ('a, 's) t =
  { start_from_emit : bool
  ; linear_unit_info : ('a, 's) Linear_format.linear_unit_info
  }

let reset () =
  { start_from_emit = false
  ; linear_unit_info =
      { unit_name = Compilenv.current_unit_name ()
      ; items = []
      ; for_pack = !Clflags.for_package
      }
  }

let should_save_before_emit t =
  should_save_ir_after Compiler_pass.Scheduling && (not t.start_from_emit)

(*
let linear_unit_info =
  { Linear_format.unit_name = "";
    items = [];
    for_pack = None;
  }
   *)

  (*
  start_from_emit := false;
  if should_save_before_emit () then begin
    linear_unit_info.unit_name <- Compilenv.current_unit_name ();
    linear_unit_info.items <- [];
    linear_unit_info.for_pack <- !Clflags.for_package;
  end
     *)

let save_data t dl =
  if should_save_before_emit t then begin
    t.linear_unit_info.items <- Linear_format.(Data dl) :: t.linear_unit_info.items
  end;
  dl

let save_linear t f =
  if should_save_before_emit t then begin
    t.linear_unit_info.items <- Linear_format.(Func f) :: t.linear_unit_info.items
  end;
  f

let write_linear t prefix =
  if should_save_before_emit t then begin
    let filename = Compiler_pass.(to_output_filename Scheduling ~prefix) in
    t.linear_unit_info.items <- List.rev t.linear_unit_info.items;
    Linear_format.save filename t.linear_unit_info
  end

let should_emit () =
  not (should_stop_after Compiler_pass.Scheduling)

let if_emit_do f x = if should_emit () then f x else ()
let emit_begin_assembly (type a s) (module Emit : Emit_intf.S with type addressing_mode = a and type specific_operation = s) = if_emit_do Emit.begin_assembly
let emit_end_assembly (type a s) (module Emit : Emit_intf.S with type addressing_mode = a and type specific_operation = s) = if_emit_do Emit.end_assembly
let emit_data (type a s) (module Emit : Emit_intf.S with type addressing_mode = a and type specific_operation = s) = if_emit_do Emit.data
let emit_fundecl
    (type a s)
    (module Emit : Emit_intf.S
      with type addressing_mode = a
       and type specific_operation = s)
    fd
  =
  if should_emit() then begin
    try
      Profile.record ~accumulate:true "emit" Emit.fundecl fd
    with Emitaux.Error e ->
      raise (Error (Asm_generation(fd.Linear.fun_name, e)))
  end

let rec regalloc
  : 'a 's .
    (module Platform_intf.S
      with type Arch.addressing_mode = 'a
       and type Arch.specific_operation = 's
    )
    -> (module Proc_intf.S
         with type addressing_mode = 'a
          and type specific_operation = 's
       )
    -> (module Printlinear.S
         with type addressing_mode = 'a
          and type specific_operation = 's
       )
    -> ppf_dump:_
    -> _
    -> ('a, 's) Mach.fundecl
    -> ('a, 's) Mach.fundecl 
  =
  fun
    (type a s)
    ((module Platform : Platform_intf.S
       with type Arch.addressing_mode = a
        and type Arch.specific_operation = s
     ) as platform)
    proc
    ((module Printlinear : Printlinear.S
       with type addressing_mode = a
        and type specific_operation = s
     ) as printlinear)
    ~ppf_dump
    round
    fd
    ->
      let module Printmach = Printlinear.Printmach in
      if round > 50 then
        fatal_error(fd.Mach.fun_name ^
                    ": function too complex, cannot complete register allocation");
      dump_if printlinear ppf_dump dump_live "Liveness analysis" fd;
      let num_stack_slots =
        if !use_linscan then begin
          (* Linear Scan *)
          Interval.build_intervals proc fd;
          if !dump_interval then Printmach.intervals ppf_dump ();
          Linscan.allocate_registers proc
        end else begin
          (* Graph Coloring *)
          Interf.build_graph proc fd;
          if !dump_interf then Printmach.interferences ppf_dump ();
          if !dump_prefer then Printmach.preferences ppf_dump ();
          Coloring.allocate_registers proc
        end
      in
      dump_if printlinear ppf_dump dump_regalloc "After register allocation" fd;
      let (newfd, redo_regalloc) = Platform.Reload.fundecl fd num_stack_slots in
      dump_if printlinear ppf_dump dump_reload "After insertion of reloading code" newfd;
      if redo_regalloc then begin
        Reg.reinit();
        Liveness.fundecl platform newfd;
        regalloc platform proc printlinear ~ppf_dump (round + 1) newfd
      end else newfd

let (++) x f = f x

let compile_fundecl
    (type a s)
    t
    ((module Platform
      : Platform_intf.S
        with type Arch.addressing_mode = a
         and type Arch.specific_operation = s)
     as platform)
    ~ppf_dump
    ~funcnames
    fd_cmm
  =
  let open Platform in
  let printlinear =
    (module Printlinear.Make(Platform)
       : Printlinear.S
         with type addressing_mode = a
          and type specific_operation = s)
  in
  let liveness = liveness platform in
  Proc.init ();
  Reg.reset();
  fd_cmm
  ++ Profile.record ~accumulate:true "cmm_invariants" (cmm_invariants ppf_dump)
  ++ Profile.record ~accumulate:true "selection"
                    (Selection.fundecl ~future_funcnames:funcnames)
  ++ Profile.record ~accumulate:true "polling"
    (Polling.instrument_fundecl
       ~operation_can_raise:Arch.operation_can_raise
       ~future_funcnames:funcnames)
  ++ pass_dump_if printlinear ppf_dump dump_selection "After instruction selection"
  ++ Profile.record ~accumulate:true "comballoc"
    (Comballoc.fundecl ~size_addr:Arch.size_addr)
  ++ pass_dump_if printlinear ppf_dump dump_combine "After allocation combining"
  ++ Profile.record ~accumulate:true "cse" CSE.fundecl
  ++ pass_dump_if printlinear ppf_dump dump_cse "After CSE"
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ Profile.record ~accumulate:true "deadcode"
    (Deadcode.fundecl ~operation_is_pure:Arch.operation_is_pure ~regs_are_volatile:Proc.regs_are_volatile)
  ++ pass_dump_if printlinear ppf_dump dump_live "Liveness analysis"
  ++ Profile.record ~accumulate:true "spill" (Spill.fundecl platform)
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ pass_dump_if printlinear ppf_dump dump_spill "After spilling"
  ++ Profile.record ~accumulate:true "split" Split.fundecl
  ++ pass_dump_if printlinear ppf_dump dump_split "After live range splitting"
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ Profile.record ~accumulate:true "regalloc"
    (regalloc platform (module Proc) printlinear ~ppf_dump 1)
  ++ Profile.record ~accumulate:true "linearize" (Linearize.fundecl (module Proc))
  ++ pass_dump_linear_if printlinear ppf_dump dump_linear "Linearized code"
  ++ Profile.record ~accumulate:true "scheduling" Scheduling.fundecl
  ++ pass_dump_linear_if printlinear ppf_dump dump_scheduling "After instruction scheduling"
  ++ save_linear t
  ++ emit_fundecl (module Emit)

module String = Misc.Stdlib.String

let compile_data t emit dl =
  dl
  ++ save_data t
  ++ emit_data emit

let compile_phrases
    (type a s)
    t
    (platform : (module Platform_intf.S
     with type Arch.addressing_mode = a
      and type Arch.specific_operation = s))
    ~ppf_dump
    ps
  =
  let funcnames =
    List.fold_left (fun s p ->
        match p with
        | Cfunction fd -> String.Set.add fd.fun_name s
        | Cdata _ -> s)
      String.Set.empty ps
  in
  let rec compile ~funcnames ps =
    match ps with
    | [] -> ()
    | p :: ps ->
       if !dump_cmm then fprintf ppf_dump "%a@." Printcmm.phrase p;
       match p with
       | Cfunction fd ->
           compile_fundecl 
             t platform
             ~ppf_dump ~funcnames fd;
          compile ~funcnames:(String.Set.remove fd.fun_name funcnames) ps
       | Cdata dl ->
           let (module Platform) = platform in
          compile_data t (module Platform.Emit) dl;
          compile ~funcnames ps
  in
  compile ~funcnames ps

let compile_phrase t platform ~ppf_dump p =
  compile_phrases t platform ~ppf_dump [p]

(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns t platform ~ppf_dump f ~generic_functions =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase t platform ~ppf_dump ph
       | _ -> ())
    (generic_functions true [Compilenv.current_unit_infos ()])

let compile_unit
    (type a s)
    ((module Platform) : (module Platform_intf.S
     with type Arch.addressing_mode = a
      and type Arch.specific_operation = s))
    ~output_prefix ~asm_filename ~keep_asm ~obj_filename gen =
  let t = reset () in
  let create_asm = should_emit () &&
                   (keep_asm || not !Emitaux.binary_backend_available) in
  Emitaux.create_asm_file := create_asm;
  Misc.try_finally
    ~exceptionally:(fun () -> remove_file obj_filename)
    (fun () ->
       if create_asm then Emitaux.output_channel := open_out asm_filename;
       Misc.try_finally
         (fun () ->
            gen t;
            write_linear t output_prefix)
         ~always:(fun () ->
             if create_asm then close_out !Emitaux.output_channel)
         ~exceptionally:(fun () ->
             if create_asm && not keep_asm then remove_file asm_filename);
       if should_emit () then begin
         let assemble_result =
           Profile.record "assemble"
             (Platform.Proc.assemble_file asm_filename) obj_filename
         in
         if assemble_result <> 0
         then raise(Error(Assembler_error asm_filename));
       end;
       if create_asm && not keep_asm then remove_file asm_filename
    )

let end_gen_implementation
    (type a s)
    t
    ((module Platform) as platform : (module Platform_intf.S
     with type Arch.addressing_mode = a
      and type Arch.specific_operation = s))
    ?toplevel
    ~ppf_dump
    (clambda : Clambda.with_constants)
  =
  let module Cmm_helpers = Cmm_helpers.Make(Platform) in
  emit_begin_assembly (module Platform.Emit) ();
  clambda
  ++ Profile.record "cmm" (Cmmgen.compunit platform)
  ++ Profile.record "compile_phrases" (compile_phrases t platform ~ppf_dump)
  ++ (fun () -> ());
  (match toplevel with
     None -> ()
   | Some f -> compile_genfuns ~generic_functions:Cmm_helpers.generic_functions t platform ~ppf_dump f);
  (* We add explicit references to external primitive symbols.  This
     is to ensure that the object files that define these symbols,
     when part of a C library, won't be discarded by the linker.
     This is important if a module that uses such a symbol is later
     dynlinked. *)
  compile_phrase t platform ~ppf_dump
    (Cmm_helpers.reference_symbols
       (List.filter_map (fun prim ->
           if not (Primitive.native_name_is_external prim) then None
           else Some (Primitive.native_name prim))
          !Translmod.primitive_declarations));
  emit_end_assembly (module Platform.Emit) ()

type middle_end =
     backend:(module Backend_intf.S)
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> Clambda.with_constants

let asm_filename output_prefix =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then output_prefix ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm

let compile_implementation
    (type a s)
    (platform : (module Platform_intf.S
                                       with type Arch.addressing_mode = a
                                        and type Arch.specific_operation = s))
    ?toplevel ~backend ~prefixname ~middle_end
      ~ppf_dump (program : Lambda.program) =
  compile_unit platform ~output_prefix:prefixname
    ~asm_filename:(asm_filename prefixname) ~keep_asm:!keep_asm_file
    ~obj_filename:(prefixname ^ ext_obj)
    (fun t ->
      Ident.Set.iter Compilenv.require_global program.required_globals;
      let clambda_with_constants =
        middle_end ~backend ~prefixname ~ppf_dump program
      in
      end_gen_implementation t platform ?toplevel ~ppf_dump clambda_with_constants)

let linear_gen_implementation
    (type a s)
    (emit : (module Emit_intf.S with type addressing_mode = a and type specific_operation = s))
    filename
  =
  let open Linear_format in
  let linear_unit_info, _ = restore filename in
  (match !Clflags.for_package, linear_unit_info.for_pack with
   | None, None -> ()
   | Some expected, Some saved when String.equal expected saved -> ()
   | _, saved -> raise(Error(Mismatched_for_pack saved)));
  let emit_item = function
    | Data dl -> emit_data emit dl
    | Func f -> emit_fundecl emit f
  in
  start_from_emit := true;
  emit_begin_assembly emit ();
  Profile.record "Emit" (List.iter emit_item) linear_unit_info.items;
  emit_end_assembly emit ()

let compile_implementation_linear
    (type a s)
    ((module Platform) as platform : (module Platform_intf.S
     with type Arch.addressing_mode = a
      and type Arch.specific_operation = s))
    output_prefix ~progname
  =
  compile_unit
    platform
    ~output_prefix
    ~asm_filename:(asm_filename output_prefix) ~keep_asm:!keep_asm_file
    ~obj_filename:(output_prefix ^ ext_obj)
    (fun _t ->
      linear_gen_implementation (module Platform.Emit) progname)

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
      fprintf ppf "Assembler error, input left in file %a"
        Location.print_filename file
  | Mismatched_for_pack saved ->
    let msg = function
       | None -> "without -for-pack"
       | Some s -> "with -for-pack "^s
     in
     fprintf ppf
       "This input file cannot be compiled %s: it was generated %s."
       (msg !Clflags.for_package) (msg saved)
  | Asm_generation(fn, err) ->
     fprintf ppf
       "Error producing assembly code for function %s: %a"
       fn Emitaux.report_error err

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let compile_phrase platform ~ppf_dump phrase = 
  let t = reset () in
  compile_phrase t platform ~ppf_dump phrase

let compile_unit
  platform
  ~output_prefix ~asm_filename ~keep_asm ~obj_filename gen
  =
  compile_unit
  platform
  ~output_prefix ~asm_filename ~keep_asm ~obj_filename
  (fun _ -> gen ())
