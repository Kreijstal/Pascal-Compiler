.section .text
.globl _start

_start:
    # Initialization: Set up the loop counter.
    # We will use the ECX register as our counter 'i'.
    movl $1, %ecx      # i = 1

loop_start:
    # --- Loop Body ---
    # This is where you would put the code that you want to execute
    # in each iteration of the loop. For this example, we will just
    # use a 'nop' (no operation) instruction as a placeholder.
    nop

    # --- Update ---
    # Increment the loop counter.
    addl $1, %ecx      # i++

    # --- Condition ---
    # Compare the loop counter 'i' with the final value, 10.
    cmpl $10, %ecx     # is i <= 10?

    # --- Conditional Jump ---
    # If the condition is met (i <= 10), jump back to the start of the loop.
    jle loop_start     # Jump if Less than or Equal

    # --- After the loop ---
    # The loop has finished. Now, we exit the program gracefully.
    movl $60, %eax     # Use syscall number 60 for 'exit'.
    xorl %edi, %edi    # Use exit code 0.
    syscall            # Make the system call to exit.
