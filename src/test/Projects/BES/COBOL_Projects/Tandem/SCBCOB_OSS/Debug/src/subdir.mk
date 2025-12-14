################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
COB_SRCS += \
../src/scbcon.cob 

OBJS += \
./src/scbcon.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cob src/subdir.mk
	@echo 'Building file: $<'
	@echo 'Invoking: COBOL Compiler (TNS/X)'
	xcobol -I"C:\GitHub\ade-nsdee_qa\Test_Projects\BES_Tests\COBOL Project\Tandem\SCBCOB_OSS\src" -g -Woptimize=1 -Wsystype=oss -Wcall_shared -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


clean: clean-src

clean-src:
	-$(RM) ./src/scbcon.o

.PHONY: clean-src

