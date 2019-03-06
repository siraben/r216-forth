;;; Register allocations
;;; r0: Top of stack (TOS)
;;; r1: Forth instruction pointer (IP)
;;; r2: Return stack pointer (RSP)
;;; r3: User pointer (HERE)

;;; r10: Terminal port
;;; sp: Parameter stack pointer (PSP)

;;; r4 - r9: unassigned
;;; r11 - r13: unassigned

;;; Hidden flag is 64
;;; Immediate flag is 128
;;; Length mask is 31

;;; word header:
;;; 0: previous entry
;;; 1: length + flags
;;; 2-4: first three characters of name
;;; 5 onwards: data


start:
        ;; Check if this is a reboot.
        mov r0, [rebooted]
        jnz did_reboot
        ;; If not, let's set up the HERE pointer.
        mov r3, here_start
did_reboot:
        ;; Regardless of whether or not we rebooted, we do the
        ;; following.
        ;; Set up stack pointer (adjusts automatically to memory space)
        mov sp, 0
        push 0
        mov [stack_zero_prog], sp
        ;; User stack zero
        push 0
        mov [stack_zero], sp
        ;; And return stack
        mov r2, 0x1f00

        ;; In case we call EXIT from the top level.
        sub r2, 1
        mov [r2], interpret_done

        mov r10, 0
        bump r10
        send r10, 0x200F

        mov r1, main
        jmp next

rebooted:
        dw 0

input_was_str:
        dw "Input was ",0

        ;; Our "stack zero" to protect against underflow.
stack_zero_prog:
        dw 0

stack_zero:
        dw 0
main:
        ;; dw lit, word_buffer, lit, input_ptr, store
        dw lit, rebooted, fetch
        dw lit, 1, lit, rebooted, store
        dw zjump, main_cont
clear_reboot:
        dw page
main_cont:
        dw lit, welcome_msg, puts
        dw lit, 0x200F, term_send
        dw lit, 0x1020, term_send

        ;; dw lit, input_was_str, puts
        dw lit, inputdata_prompt, puts

        dw lit, str_buffer, lit, 128, lit, 0x1022, getline
        dw page
        dw lit, str_buffer, lit, input_ptr, store
        ;; dw lit, sample, puts
        ;; dw lit, 0x1060, term_send
interpret_loop:
        dw check_underflow
        dw word, qdup, zjump, interpret_done
        dw find, qdup, zjump, maybe_number
        dw state, fetch, zjump, interpret_word
compiling_word:
        dw dup, qimmed, zjump, compile_word
        ;; Word is immediate, special yellow color
        dw lit, 0x200E, term_send
interpret_word:
        ;; dw lit, word_buffer, puts, space
        dw to_cfa, execute
        dw jump, interpret_loop
compile_word:
        ;; dw lit, word_buffer, puts, space
        dw to_cfa, comma, jump, interpret_loop

maybe_number:
        dw lit, word_buffer, number
        dw lit, num_status, fetch, zjump, not_found
        dw state, fetch, zjump, interpret_number
compile_number:
        dw lit, lit, comma, comma
        ;; dw lit, 0x2009, term_send
        ;; dw lit, word_buffer, puts, space
        dw jump, interpret_loop
interpret_number:
        ;; dw lit, 0x200B, term_send
        ;; dw lit, word_buffer, puts, space
        dw jump, interpret_loop

colorize_state:
        call docol
        dw state, fetch, zjump, colorize_interp
        ;; red for compiling
        dw lit, 0x200C, term_send
        dw exit
colorize_interp:
        ;; green for interpreting
        dw lit, 0x200A, term_send
        dw exit

;; (IP) -> W
;; IP + 1 -> IP
;; JP (W)
next:
        mov r4, [r1]
        add r1, 1
        jmp r4

;; PUSH_IP_RS
;; POP IP
;; JP NEXT
docol:
        sub r2, 1
        mov [r2], r1
        pop r1
        jmp next

;; POP_IP_RS
;; JP NEXT
exit:
        mov r1, [r2]
        add r2, 1
        jmp next

done_msg:
        dw 0x200F, " ok", 0
interpret_done:
        dw lit, done_msg, puts
        dw halt
sz_link:
        dw 0
        dw 2, "s0 "
sz:
        push r0
        mov r0, [stack_zero]
        jmp next

sp_fetch_link:
        dw sz_link
        dw "sp@"
sp_fetch:
        push r0
        mov r0, sp
        jmp next

depth_link:
        dw sp_fetch_link
        dw 5, "dep"
depth:
        call docol
        dw sz, sp_fetch, minus, two_minus, exit

print_stack_link:
        dw depth_link
        dw 2, ".s "
print_stack:
        call docol
        dw lit, '<', emit, depth, u_dot_, lit, '>', emit, space
        dw sp_fetch
print_stack_loop:
        dw dup, sz, one_minus, less_than
        dw zjump, print_stack_done

        dw dup, fetch, u_dot, one_plus
        dw jump, print_stack_loop
print_stack_done:
        dw drop, exit

not_found:
        dw lit, word_buffer, puts
        dw lit, not_found_msg, puts, halt

check_underflow:
        cmp sp, [stack_zero_prog]
        je report_underflow
        jmp next
report_underflow:
        mov r0, stack_underflow_msg
        call write_string
        hlt

stack_underflow_msg:
        dw 0x1000, 0x200C, "Stack underflow.", 0

welcome_msg:
        dw 0x1000, 0x200E, "System booted.", 0

inputdata_prompt:
        dw 0x1020, 0x200F, "> ", 0
        dw 0

        ;; DATA
var_base:
        dw 10

latest_link:
        dw print_stack_link
        dw 6, "lat"
        ;; CODE
latest:
        push r0
        mov r0, var_latest
        jmp next

base_link:
        dw latest_link
        dw 4, "bas"
        ;; CODE
base:
        push r0
        mov r0, var_base
        jmp next

bool_and_link:
        dw base_link
        dw 3, "and"
        ;; CODE
bool_and:
        pop r4
        and r0, r4
        jmp next

;; strcmp:
;;         pop r4
;;         mov r5, [r0 + 0]
;;         cmp r5, [r4 + 0]
;;         jnz false
;;         mov r5, [r0 + 1]
;;         cmp r5, [r4 + 1]
;;         jnz false
;;         mov r5, [r0 + 2]
;;         cmp r5, [r4 + 2]
;;         jnz false
;;         jmp true

        ;; Find a word
        ;; ( str_addr len -- xt | 0 )
        ;; CODE
find_link:
        dw bool_and_link
        dw 4, "fin"
find:
        ;; String length
        mov r9, r0
        pop r11
        ;; r4 points to the entry we're searching
        ;; Get the address of the latest word and skip the link
        ;; pointer.
        mov r4, [var_latest]
find_restart:
        mov r5, r4
        add r5, 1
        mov r5, [r5]
        ;; Check if hidden.
        ands r5, 64
        ;; Yes, skip it and continue traversing the linked list.
        jnz find_loop
        ;; No, check length.
        ;; Remove flag data except for length.
        and r5, 31
        ;; Same length?
        cmp r5, r9
        ;; Yes, compare strings.
        je find_cmp_string
        ;; No, continue searching.
        jmp find_loop
find_cmp_string:
        mov r7, r11
        mov r6, r4
        add r6, 2
        ;; r6 now points at the beginning of the name field
        mov r8, [r6 + 0]
        cmp r8, [r7 + 0]
        jne find_loop
        cmp r9, 1
        je find_succ

        mov r8, [r6 + 1]
        cmp r8, [r7 + 1]
        jne find_loop
        cmp r9, 2
        je find_succ

        mov r8, [r6 + 2]
        cmp r8, [r7 + 2]
        jne find_loop
        ;; We found it!
find_succ:
        mov r0, r4
        jmp next

find_loop:
        ;; Deference the pointer
        mov r4, [r4]
        ;; Hit null pointer.
        jz false
        jmp find_restart


        ;; CODE
allot_link:
        dw find_link
        dw 5, "all"
allot:
        add r3, r0
        pop r0
        jmp next

        ;; CODE
here_link:
        dw allot_link
        dw 4, "her"
here:
        push r0
        mov r0, r3
        jmp next

fetch_link:
        dw here_link
        dw 1, "@  "
        ;; CODE
fetch:
        mov r4, [r0]
        mov r0, r4
        jmp next

store_link:
        dw fetch_link
        dw 1, "!  "
        ;; CODE
store:
        pop r4
        mov [r0], r4
        pop r0
        jmp next

comma_link:
        dw store_link
        dw 1, ",  "
comma:
        mov [r3], r0
        pop r0
        add r3, 1
        jmp next

plus_store_link:
        dw comma_link
        dw 2, "+! "
        ;; CODE
plus_store:
        pop r4
        add [r0], r4
        pop r0
        jmp next

minus_store_link:
        dw plus_store_link
        dw 2, "-! "
        ;; CODE
minus_store:
        pop r4
        sub [r0], r4
        pop r0
        jmp next

        ;; unsigned divide r4 by r5 (doesn't handle division by 0)
        ;; quotient is r4, remainder is r6; clobbers r7 and r8
        ;; CODE
udiv1616:
        mov r6, 0
        mov r7, 0
        mov r8, 16
.loop:
        shl r7, 1
        add r4, r4
        adc r6, r6
        jc .subtract_due_to_carry
        cmp r6, r5
        jnae .no_subtract
.subtract_due_to_carry:
        sub r6, r5
        or r7, 1
.no_subtract:
        sub r8, 1
        jnz .loop
        mov r4, r7
        ret

to_r_link:
        dw minus_store_link
        dw 2, ">r "
to_r:
        sub r2, 1
        mov [r2], r0
        pop r0
        jmp next

from_r_link:
        dw to_r_link
        dw 2, "r> "
from_r:
        push r0
        mov r0, [r2]
        add r2, 1
        jmp next

div_mod_link:
        dw from_r_link
        dw 4, "/mo"
div_mod:
        pop r4
        mov r5, r0
        call udiv1616
        mov r0, r4
        push r6
        jmp next

left_shift:
        add r0, r0
        jmp next

right_shift:
        shr r0, 1
        jmp next

nip_link:
        dw div_mod_link
        dw 3, "nip"
nip:
        pop r4
        jmp next

mod_link:
        dw nip_link
        dw 3, "mod"
mod:
        call docol
        dw div_mod, drop, exit

div_link:
        dw mod_link
        dw 3, "div"
div:
        call docol
        dw div_mod, nip, exit

space_link:
        dw div_link
        dw 5, "spa"
space:
        send r10, 32
        jmp next

u_dot_:
        call docol
        dw base, fetch, div_mod, qdup, zbranch, 2, u_dot_
        dw dup, lit, 10, less_than, zbranch, 5, lit, '0'
        dw branch, 6, lit, 10, minus, lit, 'A', plus
        dw emit
        dw exit

uwidth:
        call docol
        dw base, fetch, div, qdup, zbranch, 5, uwidth, one_plus
        dw branch, 3, lit, 1, exit

;;; Writing instructions means that we have to access the full 29 bits
;;; Diagram of the situation:
;;; SWM (set write mask) takes the least 13 significant bits from the
;;; operand and sets the write mask to that

;;; SWM  xxxnnnnnnnnnnnnn
;;;         \-----------/
;;;               |
;;;               +-- [ part that gets written into the write mask ]

;;; Let's say DOCOL's address is 593 in base 10, then.
;;; The bits of the cell that corresponds to CALL DOCOL should look
;;; like this:
;;;     111110001000000010010100010000
;;;     \------------/\--------------/
;;;         |             |
;;; [CALL opcode]         +- [ address of DOCOL left shifted by 4 ]

;;; But here's the challenge: we can only write to the first 16 bits!
;;;

;;;
;;;   [ the part SWM can write to ]
;;;           |
;;;           |             +--- [ the part we can write to ]
;;;           |             |
;;;           |             |
;;;           |             |
;;;     /------------\/--------------\
;;;     111110001000000010010100010000
;;;     \------------/\--------------/
;;;         |           |
;;; [CALL opcode]       +----- [ address of DOCOL ]

;;; If DOCOL's address is high enough we'll need to handle that, as
;;; a couple of its significant bits might end up in the SWM region,
;;; but to keep it simple let's make sure DOCOL is below 2^11

        ;; ( addr length -- )
        ;; Parse the next word and create a definition header for it.
create_:
        ;; Write the link pointer first
        mov r4, [var_latest]
        mov [r3], r4
        mov [var_latest], r3
        mov r4, docol
        ;; Main cell content
        shl r4, 4

        swm 15904
        ;; Write CALL DOCOL at offset 5
        mov [r3 + 5], r4
        ;; Reset write mask
        swm 0

        ;; Write the length
        mov [r3 + 1], r0
        pop r0
        ;; Write the first three characters of the name
        mov r5, [r0]
        mov [r3 + 2], r5
        mov r5, [r0 + 1]
        mov [r3 + 3], r5
        mov r5, [r0 + 2]
        mov [r3 + 4], r5

        pop r0
        add r3, 6
        jmp next

create_link:
        dw space_link
        dw 6, "cre"
create:
        call docol
        dw word
        ;; DEBUG PRINT
        ;; dw lit, word_buffer, puts, space
        dw create_, exit

immed_link:
        dw create_link
        dw 5, "imm"
immed:
        ands [r0], 128
        jnz true
        jz false

qimmed_link:
        dw immed_link
        dw 6, "?im"
qimmed:
        mov r0, [r0+1]
        ands r0, 128
        jnz true
        jz false

hidden_link:
        dw qimmed_link
        dw 6, "hid"
hidden:
        mov r4, r0
        add r4, 1
        xor [r4], 64
        pop r0
        jmp next

        ;; IMMEDIATE
lbrac_link:
        dw hidden_link
        dw 129, "[  "
lbrac:
        mov [var_state], 0
        jmp next

rbrac_link:
        dw lbrac_link
        dw 1, "]  "
rbrac:
        mov [var_state], 1
        jmp next

colon_link:
        dw rbrac_link
        dw 1, ":  "
colon:
        call docol
        dw create, latest, fetch
        dw hidden
        dw rbrac, exit

        ;; IMMEDIATE
semicolon_link:
        dw colon_link
        dw 129, ";  "
semicolon:
        call docol
        dw lit, exit, comma
        dw latest, fetch
        dw hidden
        dw lbrac, exit

dot_link:
        dw semicolon_link
        dw 1, ".  "
dot:
        jmp d_dot

;;; Much faster than U. but only for base 10.
d_dot_link:
        dw dot_link
        dw 2, "d. "
d_dot:
;;; Written by LBPHacker
;;; unsigned render r4 into zero-terminated bcd
;;; r5 is set to point to result (owned by the subroutine, don't write to it)
;;; clobbers r4, r6, r7 and r8
tozstringu16:
        mov r4, r0
        test r4, r4
        jz .r4_zero
        mov r5, 0
        mov r6, 0
        mov r7, 16
.loop:
        shl r4, 1
        scl r5, 1
        scl r6, 1
        sub r7, 1
        jz .loop_done
        mov r8, r5
        ror r5, 1
        or r8, r5
        ror r5, 1
        and r8, r5
        ror r5, 1
        or r8, r5
        rol r5, 3
        and r8, 0x1111
        add r5, r8
        add r5, r8
        add r5, r8
        jmp .loop
.loop_done:
        mov r7, .output_4
        mov [r7], r5
        and [r7], 15
        add [r7], '0'
        shr r5, 4
        sub r7, 1
        mov [r7], r5
        and [r7], 15
        add [r7], '0'
        shr r5, 4
        sub r7, 1
        mov [r7], r5
        and [r7], 15
        add [r7], '0'
        shr r5, 4
        add r5, '0'
        sub r7, 1
        mov [r7], r5
        add r6, '0'
        sub r7, 1
        mov [r7], r6
        mov r5, r7
.find_head:
        cmp [r5], '0'
        jne .done
        add r5, 1
        jmp .find_head
.done:
        jmp print_d_dot
.r4_zero:
        mov r5, .output_0
        add r5, 4
        mov [r5], '0'
        jmp print_d_dot
.output_0: dw 0
.output_1: dw 0
.output_2: dw 0
.output_3: dw 0
.output_4: dw 0
.output_z: dw 0

print_d_dot:
        mov r0, r5
        call write_string
        ;; Space at the end
        send r10, 32
        pop r0
        jmp next

u_dot_link:
        dw d_dot_link
        dw 2, "u. "
u_dot:
        call docol
        dw u_dot_, space, exit

qdup_link:
        dw u_dot_link
        dw 4, "?du"
qdup:
        cmp r0, 0
        je next
        push r0
        jmp next

true:
        mov r0, 1
        jmp next

false:
        mov r0, 0
        jmp next

less_than_link:
        dw qdup_link
        dw 1, "<  "
less_than:
        pop r4
        cmp r4, r0
        jl  true
        jmp false

equal_link:
        dw less_than_link
        dw 1, "=  "
equal:
        pop r4
        cmp r4, r0
        je  true
        jmp false

not_equal_link:
        dw equal_link
        dw 2, "<> "
not_equal:
        pop r4
        cmp r4, r0
        jne true
        jmp false

dup_link:
        dw not_equal_link
        dw 3, "dup"
dup:
        push r0
        jmp next

two_dup_link:
        dw dup_link
        dw 4, "2du"
two_dup:
        push r0
        push [sp+1]
        jmp next

drop_link:
        dw two_dup_link
        dw 4, "dro"
drop:
        pop r0
        jmp next

two_drop_link:
        dw drop_link
        dw 5, "2dr"
two_drop:
        pop r0
        pop r0
        jmp next

swap_link:
        dw two_drop_link
        dw 4, "swa"
swap:
        pop r5
        push r0
        mov r0, r5
        jmp next

plus_link:
        dw swap_link
        dw 1, "+  "
plus:
        pop r4
        add r0, r4
        jmp next

minus_link:
        dw plus_link
        dw 1, "-  "
minus:
        mov r4, r0
        pop r0
        sub r0, r4
        jmp next

times_link:
        dw minus_link
        dw 1, "*  "
times:
;;; Written by LBPHacker
;;; unsigned multiply r4 by r5
;;; product is r4; clobbers r5, r6 and r7
umul1616_16:
        pop r5
        mov [.cache_1l], r0
        mov [.cache_3l], r0
        add r0, r0
        mov [.cache_2l], r0
        add [.cache_3l], r0
        mov r6, 8
        mov r0, 0
.loop:
        shl r0, 2
        rol r5, 2
        mov r7, r5
        and r7, 3
        add r0, [.cache_0l+r7]
        sub r6, 1
        jnz .loop
        jmp next
.cache_0l: dw 0
.cache_1l: dw 0
.cache_2l: dw 0
.cache_3l: dw 0
double_times_link:
        dw times_link
        dw 3, "um*"
double_times:
;;; Written by LBPHacker
;;; unsigned multiply r4 by r5
;;; product is r4_32; clobbers r6, r7 and r8
umul1616:
        mov r4, r0
        pop r5
        mov [.cache_1l], r5
        mov [.cache_3l], r5
        mov [.cache_3h], 0
        mov r6, 0
        add r5, r5
        adc r6, 0
        mov [.cache_2l], r5
        mov [.cache_2h], r6
        add [.cache_3l], r5
        adc [.cache_3h], r6
        mov r7, r4
        mov r6, 8
        mov r5, 0
        mov r4, 0
.loop:
        shl r4, 2
        scl r5, 2
        rol r7, 2
        mov r8, r7
        and r8, 3
        add r4, [.cache_0l+r8]
        adc r5, [.cache_0h+r8]
        sub r6, 1
        jnz .loop
        push r4
        mov r0, r5
        jmp next
.cache_0l: dw 0
.cache_1l: dw 0
.cache_2l: dw 0
.cache_3l: dw 0
.cache_0h: dw 0
.cache_1h: dw 0
.cache_2h: dw 0
.cache_3h: dw 0

one_minus_link:
        dw double_times_link
        dw 2, "1- "
one_minus:
        sub r0, 1
        jmp next

one_plus_link:
        dw one_minus_link
        dw 2, "1+ "
one_plus:
        add r0, 1
        jmp next

two_minus_link:
        dw one_plus_link
        dw 2, "2- "
two_minus:
        sub r0, 2
        jmp next

two_plus_link:
        dw two_minus_link
        dw 2, "2+ "
two_plus:
        add r0, 2
        jmp next

branch:
        mov r4, [r1]
        add r1, r4
        jmp next

zbranch:
        cmp r0, 0
        je zbranch_succ
        pop r0
        add r1, 1
        jmp next

zbranch_succ:
        pop r0
        mov r4, [r1]
        add r1, r4
        jmp next



jump:
        mov r4, [r1]
        mov r1, r4
        jmp next

zjump:
        cmp r0, 0
        je zjump_succ
        pop r0
        add r1, 1
        jmp next

zjump_succ:
        pop r0
        mov r4, [r1]
        add r1, 1
        mov r1, r4
        jmp next


;; ( addr count cursor -- )
getline:
        mov r11, r0
        mov r6, r1
        pop r1
        mov r7, 0x200F
        pop r0

        ;; Save r3, r1
        push r3
        push r6
        call read_string
        ;; Restore r1, r3
        pop r1
        pop r3
        ;; New TOS
        pop r0
        jmp next

        ;; GETC to be called via other Forth words
getc_forth:
        call docol
        dw lit, input_ptr, fetch, fetch
        dw lit, 1, lit, input_ptr, plus_store
        dw exit

        ;; GETC to be called by assembly
        ;; Stores result in r5
        ;; Clobbers r9
getc_asm:
        mov r9, [input_ptr]
        mov r5, [r9]
        add r9, 1
        mov [input_ptr], r9
        ret

;;; 1 is success, 0 is failure
num_status:
        dw 0

number_link:
        dw two_plus_link
        dw 12, "num"
number:
;;; Written by LBPHacker
;;; unsigned parse zero-terminated bcd pointed to by r4 into r5
;;; carry flag is set if something goes wrong, clear otherwise
;;; clobbers r4, r6 and r7
        mov r4, r0
fromzstringu16:
        mov r5, 0
        cmp [r4], 0
        je .err_empty
.loop:
        mov r6, [r4]
        test r6, r6
        jnz .not_done
        mov r4, 0
        jmp .done
.not_done:
        add r4, 1
        sub r6, '0'
        jc .err_nondigit
        cmp r6, 9
        ja .err_nondigit
        cmp r5, 6553
        ja .err_overflow
        shl r5, 1
        mov r7, r5
        shl r5, 2
        add r5, r7
        add r5, r6
        jc .err_overflow
        jmp .loop
.err_empty:
.err_nondigit:
.err_overflow:
        mov [num_status], 0
        jmp next
.done:
        mov [num_status], 1
        add r4, 0xFFFF
        mov r0, r5
        jmp next


word_ptr:
        dw 0
word_buffer:
        dw "           "
word_link:
        dw number_link
        dw 4, "wor"
word:
        ;; r6: word pointer
        ;; r4: character point
        ;; r5: return from getc_asm
        ;; r7: first indirection of word_ptr
        ;; Used to be a WORD word but has been rewritten in assembly for speed.
        ;; dw lit, word_buffer, lit, word_ptr, store
        mov [word_ptr], word_buffer
        mov r7, word_buffer
        mov r4, 0
skip_space:
        call getc_asm
        ;; dw qdup, zjump, empty_word
        cmp r5, 0
        je empty_word
        ;; dw dup, lit, 32, not_equal
        ;; dw zjump, skip_space
        cmp r5, 32
        je skip_space

        ;; Possibly add more space characters to skip
        ;; dw jump, actual_word
        jmp actual_word

actual_word:
        ;; dw lit, word_ptr, fetch, store
        mov [r7], r5
        ;; Increase character count
        add r4, 1
        ;; dw lit, 1, lit, word_ptr, plus_store
        add r7, 1

actual_word_loop:
        ;; dw getc
        call getc_asm
        ;; dw dup, zjump, word_done
        cmp r5, 0
        je word_done

        ;; dw dup, lit, 32, not_equal, zjump, word_done
        cmp r5, 32
        je word_done
        ;; dw jump, actual_word
        jmp actual_word

word_done:
        push r0
        push word_buffer
        mov r0, r4
        ;; dw lit, 0, lit, word_ptr, fetch, store
        mov [r7], 0
        jmp next

empty_word:
        mov r0, 0
        jmp next

divmod_link:
        dw word_link
        dw 4, "/mo"
divmod:
        pop r5
        mov r6, 0
        mov r6, 16

        jmp next

origin_link:
        dw divmod_link
        dw 6, "ori"
origin:
        send r10, 0x1000
        jmp next

page_link:
        dw origin_link
        dw 4, "pag"
page:
        send r10, 0x1000
        mov r4, 192
page_loop:
        send r10, 32
        sub r4, 1
        jnz page_loop
        ;; Reset cursor to origin
        send r10, 0x1000
        jmp next

;;; Print a character.
;;; ( c -- )
emit_link:
        dw page_link
        dw 4, "emi"
emit:
        send r10, r0
        pop r0
        jmp next

;; Send a message to the terminal.
;; ( n -- )
term_send:
        send r10, r0
        pop r0
        jmp next

exec_msg:
        dw 0x200F, "Executing ", 0
not_found_msg:
        dw 0x200F, " not found! ", 0

to_cfa_link:
        dw emit_link
        dw 4, ">cf"
to_cfa:
        add r0, 5
        jmp next

execute_link:
        dw to_cfa_link
        dw 7, "exe"
execute:
        mov r4, r0
        pop r0
        jmp r4

recurse_link:
        dw execute_link
        dw 7, "rec"
recurse:
        call docol
        dw latest, fetch, to_cfa, exit

puts_link:
        dw recurse_link
        dw 4, "put"
puts:
        call write_string
        pop r0
        jmp next


lit_link:
        dw puts_link
        dw 3, "lit"
lit:
        push r0
        mov r0, [r1]
        add r1, 1
        jmp next

halt_link:
        dw lit_link
        dw 4, "hal"
halt:
        hlt

; * Writes zero-terminated strings to the terminal.
; * r0 points to buffer to write from.
; * r10 is terminal port address.
; * r11 is incremented by the number of characters sent to the terminal (which
;   doesn't help at all if the string contains colour or cursor codes).
write_string:
        push r0
        push r1
        mov r5, r0
.loop:
        mov r1, [r0]
        jz .exit
        add r0, 1
        send r10, r1
        jmp .loop
.exit:
        add r11, r0
        sub r11, r5
        pop r1
        pop r0
        ret

; * Sends spaces to the terminal.
; * r10 holds the number of spaces to send.
clear_continuous:
.loop:
    send r10, 32
    sub r0, 1
    jnz .loop
    ret


; * Reads a single character from the terminal.
; * Character code is returned in r0.
; * r10 is terminal port address.
read_character:
.wait_loop:
    wait r3                   ; * Wait for a bump. r3 should be checked but
                              ;   as in this demo there's no other peripheral,
                              ;   it's fine this way.
    js .wait_loop
    bump r10                  ; * Ask for character code.
.recv_loop:
    recv r0, r10              ; * Receive character code.
    jnc .recv_loop            ; * The carry bit it set if something is received.
    ret

; * Sends spaces to the terminal.
; * r10 holds the number of spaces to send.
clear_continuous:
.loop:
    send r10, 32
    sub r0, 1
    jnz .loop
    ret

; * Reads a single character from the terminal while blinking a cursor.
; * r6 is cursor colour.
; * r10 is terminal port address.
; * r11 is cursor position.
; * Character read is returned in r8.
read_character_blink:
    mov r12, 0x7F             ; * r12 holds the current cursor character.
    mov r9, 8                 ; * r9 is the counter for the blink loop.
    send r10, r6
    send r10, r11
    send r10, r12              ; * Display cursor.
.wait_loop:
    wait r8                   ; * Wait for a bump. r3 should be checked but
                              ;   as in this demo there's no other peripheral,
                              ;   it's fine this way.
    jns .got_bump             ; * The sign flag is cleared if a bump arrives.
    sub r9, 1
    jnz .wait_loop            ; * Back to waiting if it's not time to blink yet.
    xor r12, 0x5F              ; * Turn a 0x20 into a 0x7F or vice versa.
    send r10, r6              ;   Those are ' ' and a box, respectively.
    send r10, r11
    send r10, r12              ; * Display cursor.
    mov r9, 8
    jmp .wait_loop            ; * Back to waiting, unconditionally this time.
.got_bump:
    bump r10                  ; * Ask for character code.
.recv_loop:
    recv r8, r10              ; * Receive character code.
    jnc .recv_loop            ; * The carry bit it set if something is received.
    ret




; * Reads zero-terminated strings from the terminal.
; * r0 points to buffer to read into and r1 is the size of the buffer,
;   including the zero that terminates the string. If you have a 15 cell
;   buffer, do pass 15 in r1, but expect only 14 characters to be read at most.
; * r7 is the default cursor colour (the one used when the buffer is not about
;   to overflow; when it is, the cursor changes to yellow, 0x200E).
; * r10 is terminal port address.
; * r11 is cursor position.
read_string:
        bump r10                  ; * Drop whatever is in the input buffer.
        mov r5, r1
        sub r5, 1                 ; * The size of the buffer includes the
                                  ;   terminating zero, so the character limit
                                  ;   should be one less than this size.
        mov r6, r7                ; * Reset the default cursor colour.
        mov r1, 0                 ; * r1 holds the number of characters read.
.read_character:
        call read_character_blink
        cmp r8, 13                ; * Check for the Return key.
        je .got_return
        cmp r8, 8                 ; * Check for the Backspace key.
        je .got_backspace
        cmp r5, r1                ; * Check if whatever else we got fits the buffer.
        je .read_character
        send r10, r11             ; * If it does, display it and add it to the
        send r10, r8              ;   buffer.
        add r11, 1
        mov [r0+r1], r8
        add r1, 1
        cmp r5, r1
        ja .read_character        ; * Change cursor colour to yellow if the buffer
        mov r6, 0x200E            ;   is full.
        jmp .read_character       ; * Back to waiting.
.got_backspace:
        cmp r1, 0                 ; * Only delete a character if there is at least
        je .read_character        ;   one to delete.
        mov r6, r7                ; * Reset the default cursor colour.
        send r10, r11
        send r10, 0x20            ; * Clear the previous position of the cursor.
        sub r11, 1
        sub r1, 1
        jmp .read_character       ; * Back to waiting.
.got_return:
        send r10, r11
        send r10, 0x20            ; * Clear the previous position of the cursor.
        mov [r0+r1], 0            ; * Terminate string explicitly.
        mov [input_len], r1
        ret

        ;; Same code as LIT
        ;; We use TICK to quote words instead of numeric literals.
tick_link:
        dw halt_link
        dw 3, "(')"
tick:
        push r0
        mov r0, [r1]
        add r1, 1
        jmp next

run_tick_link:
        dw tick_link
        dw 1, "'"
run_tick:
        call docol
        dw word, find, to_cfa, exit


        ;; ZERO is a dummy constant
zero_link:
        dw run_tick_link
        dw 4, "zer"
zero:
        push r0
        mov r0, 0
        jmp next

        ;; TEN is a dummy constant
ten_link:
        dw zero_link
        dw 3, "ten"
ten:
        push r0
        mov r0, 10
        jmp next

        ;; IMMEDIATE
if_link:
        dw ten_link
        dw 130, "if "
if:
        call docol
        dw tick, zbranch, comma, here, lit, 0, comma, exit

        ;; IMMEDIATE
else_link:
        dw if_link
        dw 132, "els"
else:
        call docol
        dw tick, branch, comma, here, lit, 0, comma
        dw swap, dup, here, swap, minus, swap, store, exit

        ;; IMMEDIATE
then_link:
        dw else_link
        dw 132, "the"

then:
        call docol
        dw dup, here, swap, minus, swap, store, exit

        ;; IMMEDIATE
do_link:
        dw then_link
        dw 130, "do "
do:
        call docol
        dw here, tick, to_r, comma, tick, to_r, comma, exit

        ;; IMMEDIATE
loop_link:
        dw do_link
        dw 132, "loo"
loop:
        call docol
        dw tick, from_r, comma, tick, from_r, comma, tick, one_plus
        dw comma, tick, two_dup, comma
        dw tick, equal, comma, tick, zbranch, comma, here, minus
        dw comma, tick, two_drop, comma, exit

state:
        push r0
        mov r0, var_state
        jmp next

var_state:
        dw 0
        ;; Latest word to be defined
var_latest:
        dw loop_link
        ;; Length of latest input
input_len:
        dw 0
input_ptr:
        dw str_buffer
        ;; Global string buffer.
        ;; 256 characters.
str_buffer:
        dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

here_start:
        ;; The rest of the memory is free space.
