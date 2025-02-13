module Register = struct
  type t = Virtual of string | Arch of string

  let string_of_reg = function Virtual v -> v | Arch a -> a
  let virtual_reg_count = ref 0

  let create_virtual () =
    let tmp = !virtual_reg_count in
    incr virtual_reg_count;
    Virtual (Printf.sprintf "v%d" tmp)

  (* Architectural registers, the ones described in ISA *)
  (* includes 64, 32, 16, and 8 bit registers *)
  let al = Arch "al"
  let ax = Arch "ax"
  let eax = Arch "eax"
  let rax = Arch "rax"
  let bl = Arch "bl"
  let bx = Arch "bx"
  let ebx = Arch "ebx"
  let rbx = Arch "rbx"
  let cl = Arch "cl"
  let cx = Arch "cx"
  let ecx = Arch "ecx"
  let rcx = Arch "rcx"
  let dl = Arch "dl"
  let dx = Arch "dx"
  let edx = Arch "edx"
  let rdx = Arch "rdx"
  let rsi = Arch "rsi"
  let esi = Arch "esi"
  let si = Arch "si"
  let sil = Arch "sil"
  let rdi = Arch "rdi"
  let edi = Arch "edi"
  let di = Arch "di"
  let dil = Arch "dil"
  let rbp = Arch "rbp"
  let ebp = Arch "ebp"
  let bp = Arch "bp"
  let bpl = Arch "bpl"
  let rsp = Arch "rsp"
  let esp = Arch "esp"
  let sp = Arch "sp"
  let spl = Arch "spl"
  let r8 = Arch "r8"
  let r8d = Arch "r8d"
  let r8w = Arch "r8w"
  let r8b = Arch "r8b"
  let r9 = Arch "r9"
  let r9d = Arch "r9d"
  let r9w = Arch "r9w"
  let r9b = Arch "r9b"
  let r10 = Arch "r10"
  let r10d = Arch "r10d"
  let r10w = Arch "r10w"
  let r10b = Arch "r10b"
  let r11 = Arch "r11"
  let r11d = Arch "r11d"
  let r11w = Arch "r11w"
  let r11b = Arch "r11b"
  let r12 = Arch "r12"
  let r12d = Arch "r12d"
  let r12w = Arch "r12w"
  let r12b = Arch "r12b"
  let r13 = Arch "r13"
  let r13d = Arch "r13d"
  let r13w = Arch "r13w"
  let r13b = Arch "r13b"
  let r14 = Arch "r14"
  let r14d = Arch "r14d"
  let r14w = Arch "r14w"
  let r14b = Arch "r14b"
  let r15 = Arch "r15"
  let r15d = Arch "r15d"
  let r15w = Arch "r15w"
  let r15b = Arch "r15b"
end
