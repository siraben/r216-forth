;;; r2asm("/Users/siraben/Documents/Playground/R216/forth.asm", 0xDEAD, "/Users/siraben/Documents/Playground/R216/r2asm.log")

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
        mov sp, 0
        push 1234

        mov [stack_zero], sp
        mov r2, 0x1f00
        
        sub r2, 1
        mov [r2], done

        mov r3, here_start
        
        mov r10, 0
        bump r10
        send r10, 0x200F
        
        mov r1, main
        jmp next
        
stack_zero:
        dw 0
main:
        ;; dw lit, word_buffer, lit, input_ptr, store
        dw lit, 1
        dw lit, 2
        dw lit, 3
        dw lit, 4
        dw lit, welcome_msg, puts
        dw lit, 0x200F, term_send
        ;; dw lit, inputdata_prompt, puts
        
        ;; dw lit, str_buffer, lit, 14, lit, 0x1032, getline
        dw lit, 0x1030, term_send
        dw lit, 0x1040, term_send
interpret_loop:
        dw word, qdup, zjump, halt
        dw find, qdup, zjump, not_found
        dw state, fetch, zjump, interpret_word
        
compiling_word:
        dw to_cfa, comma, jump, interpret_loop
interpret_word:        
        dw to_cfa, execute
        dw jump, interpret_loop

done_msg:
        dw 0x1090, 0x200F, "done ", 0
done:
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
        

fib:
        call docol
        dw dup, lit, 2, less_than, zjump, fib_cont
        dw exit
fib_cont:
        dw dup, one_minus, fib
        dw swap, one_minus, one_minus, fib
        dw plus, exit

not_found:
        dw not_found_msg, puts, halt
        
welcome_msg:
        dw 0x1000, 0x200E, "Welcome to R216 Forth.", 0

inputdata_prompt:
        dw 0x1030, 0x200F, "> ", 0
        dw 0
        
        ;; DATA
var_base:
        dw 10
        
        ;; CODE
base:
        push r0
        mov r0, var_base
        jmp next
        
        ;; CODE
hidden:
        ands r0, 64
        jz false
        jmp true
        
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
        mov r0, r6
        jmp next

find_loop:
        ;; Deference the pointer
        mov r4, [r4]
        ;; Hit null pointer.
        jz false
        jmp find_restart

        
        ;; CODE
allot:
        add r3, r0
        pop r0
        jmp next

        ;; CODE
here_link:
        dw print_stack_link
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

        ;; CODE
plus_store:
        pop r4
        add [r0], r4
        pop r0
        jmp next

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


to_r:
        sub r2, 1
        mov [r2], r0
        jmp next

from_r:
        push r0
        mov r0, [r2]
        add r2, 1
        jmp next
        
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
nip:
        pop r4
        jmp next
        
mod:
        call docol
        dw div_mod, drop, exit

div:
        call docol
        dw div_mod, nip, exit

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

u_dot_link:
        dw comma_link
        dw 2, "u. "
u_dot:
        call docol
        dw u_dot_, space, exit

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

less_than:
        pop r4
        cmp r4, r0
        jl  true
        jmp false

equal:  
        pop r4
        cmp r4, r0
        je  true
        jmp false

not_equal:  
        pop r4
        cmp r4, r0
        jne  true
        jmp false
        
dup:
        push r0
        jmp next

two_dup:
        ;; REFACTOR: into a single pointer indirection
        pop r4
        push r4
        push r0
        push r4
        jmp next

drop:
        pop r0
        jmp next

swap:
        pop r5
        push r0
        mov r0, r5
        jmp next

plus:
        pop r4
        add r0, r4
        jmp next


minus:
        mov r4, r0
        pop r0
        sub r0, r4
        jmp next

one_minus:
        sub r0, 1
        jmp next

two_minus:
        sub r0, 2
        jmp next
        
one_plus:
        add r0, 1
        jmp next

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

getc:
        call docol
        dw lit, input_ptr, fetch, fetch
        dw lit, 1, lit, input_ptr, plus_store
        dw exit

word_ptr:
        dw 0
word_buffer:
        dw "           "
word:
        call docol
        dw lit, word_buffer, lit, word_ptr, store
        dw lit, 0
skip_space:
        dw drop
        dw getc
        dw qdup, zjump, empty_word
        dw dup, lit, 32, not_equal
        dw zjump, skip_space
        ;; Possibly add more space characters to skip

        dw jump, actual_word

actual_word:
        dw lit, word_ptr, fetch, store
        dw lit, 1, lit, word_ptr, plus_store

actual_word_loop:
        dw getc
        dw dup, zjump, word_done
        dw dup, lit, 32, not_equal, zjump, word_done
        dw jump, actual_word

word_done:
        dw drop
        dw lit, 0, lit, word_ptr, fetch, store
        dw lit, word_ptr, fetch, lit, word_buffer, minus
        dw lit, word_buffer, dup, lit, word_ptr, store
        dw swap, exit

empty_word:
        dw lit, 0, exit

divmod:
        pop r5
        mov r6, 0
        mov r6, 16
        
        jmp next
;; Print a character.
;; ( c -- )
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


;; (IP) -> W
;; IP + 1 -> IP
;; JP (W)
next:
        mov r4, [r1]
        add r1, 1
        jmp r4

;; Since we do a call to docol, we
;; assume we have the return address
;; on the top of the stack.

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
        
exec_msg:
        dw 0x200F, "Executing ", 0
not_found_msg:
        dw 0x200F, "Word not found! ", 0

addr_msg:
        dw 0x200F, "Address: ", 0

        
to_cfa_link:
        dw u_dot_link
        dw 4, ">cf"
to_cfa:
        add r0, 3
        jmp next
        
execute_link:
        dw to_cfa_link
        dw 7, "exe"
execute:
        mov r4, r0
        pop r0
        jmp r4

puts_link:
        dw execute_link
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
        cmp r8, 13                ; * Check for thr Return key.
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


foo_link:
        dw halt_link
        dw 3, "foo"
foo:
        push r0
        mov r0, 42
        jmp next
        
star_link:
        dw foo_link
        dw 4, "sta"
star:
        call docol
        dw lit, 42, emit, exit
        

state:
        push r0
        mov r0, var_state
        jmp next
        
var_state:
        dw 0
        ;; Latest word to be defined
var_latest:
        dw star_link
        ;; Length of latest input
input_len:
        dw 0
input_ptr:
        dw sample
        ;; Global string buffer.
str_buffer:
        dw "              "   ; * Global string buffer for use with functions
                              ;   that operate on strings. 14 cells. Don't
                              ;   worry, it's thread-safe.
sample:
        ;; dw "star star star"     
        dw "here @ u. foo here ! here @ u. .s halt",0
here_start:
        ;; each row is 16 cells
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "
        dw "                "        
