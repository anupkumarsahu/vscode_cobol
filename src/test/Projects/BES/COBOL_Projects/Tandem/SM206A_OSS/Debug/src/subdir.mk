################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
COB_SRCS += \
../src/SM206A-mod.cob 

OBJS += \
./src/SM206A-mod.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cob src/subdir.mk
	@echo 'Building file: $<'
	@echo 'Invoking: COBOL Compiler (TNS/X)'
	xcobol -I"C:\GitHub\ade-nsdee_qa\Test_Projects\BES_Tests\COBOL Project\Tandem\SM206A_OSS\src" -g -Woptimize=1 -Wsystype=oss -Wcall_shared -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


clean: clean-src

clean-src:
	-$(RM) ./src/SM206A-mod.o

.PHONY: clean-src

