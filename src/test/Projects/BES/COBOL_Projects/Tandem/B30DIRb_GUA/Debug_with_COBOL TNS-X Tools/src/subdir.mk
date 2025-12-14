################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
COB_SRCS += \
../src/B30DIRb.cob 

OBJS += \
./src/B30DIRb.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cob
	@echo 'Building file: $<'
	@echo 'Invoking: COBOL Compiler (TNS/X)'
	xcobol -I"C:\Anup\NSDi\RCPTT\aut-NSDEE\B30DIRb_GUA\src" -g -Woptimize=1 -Wsystype=guardian -Wcall_shared -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


