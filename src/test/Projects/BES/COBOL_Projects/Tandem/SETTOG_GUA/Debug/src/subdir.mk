################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
COB_SRCS += \
../src/B30DIRb.cob 

OBJS += \
./src/B30DIRb.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cob src/subdir.mk
	@echo 'Building file: $<'
	@echo 'Invoking: COBOL Compiler (TNS/X)'
	xcobol -I"C:\GitHub\ade-nsdee_qa\Test_Projects\BES_Tests\COBOL Project\Tandem\SETTOG_GUA\src" -g -Woptimize=1 -Wsystype=guardian -Wcall_shared -Wxld=-allow_multiple_mains -Wxld=-allow_duplicate_procs -Wxld=\"-unres_symbols Ignore\" -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


clean: clean-src

clean-src:
	-$(RM) ./src/B30DIRb.o

.PHONY: clean-src

