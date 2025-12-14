################################################################################
# Automatically-generated file. Do not edit!
################################################################################

-include ../makefile.init

RM := rm -rf

# All of the sources participating in the build are defined here
-include sources.mk
-include src/subdir.mk
-include ../makefile.defs

OPTIONAL_TOOL_DEPS := \
$(wildcard ../makefile.defs) \
$(wildcard ../makefile.init) \
$(wildcard ../makefile.targets) \


BUILD_ARTIFACT_NAME := SM206A_OSS
BUILD_ARTIFACT_EXTENSION := txe
BUILD_ARTIFACT_PREFIX :=
BUILD_ARTIFACT := $(BUILD_ARTIFACT_PREFIX)$(BUILD_ARTIFACT_NAME)$(if $(BUILD_ARTIFACT_EXTENSION),.$(BUILD_ARTIFACT_EXTENSION),)

# Add inputs and outputs from these tool invocations to the build variables 
PHONEY_DEPLOY_OUTPUT += \
deploy \

PHONEY_PROFMRG_OUTPUT += \
profmrg \

PHONEY_CODECOV_OUTPUT += \
codecov \


# All Target
all: main-build

# Main-build Target
main-build: SM206A_OSS.txe

# Tool invocations
SM206A_OSS.txe: $(OBJS) $(USER_OBJS) makefile $(OPTIONAL_TOOL_DEPS)
	@echo 'Building target: $@'
	@echo 'Invoking: COBOL Linker (TNS/X)'
	xcobol -Wsystype=oss -Wcall_shared -o "SM206A_OSS.txe" $(OBJS) $(USER_OBJS_WOESC) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '

deploy: SM206A_OSS.txe makefile $(OPTIONAL_TOOL_DEPS)
	@echo 'Invoking: Deploy Tool'
	java -jar "$(NSDEE_TOOLS_LOC)/Deploy.jar" -q=oss -code=500 -mode=bin -p= -t= -h= -u= -r="SM206A_OSS.txe" -f="SM206A_OSS.txe"
	@echo 'Finished building: $@'
	@echo ' '

profmrg:  makefile $(OPTIONAL_TOOL_DEPS)
	@echo 'Invoking: Profmrg Tool'
	profmrg
	@echo 'Finished building: $@'
	@echo ' '

codecov: profmrg makefile $(OPTIONAL_TOOL_DEPS)
	@echo 'Invoking: Code coverage Tool'
	codecov
	@echo 'Finished building: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) SM206A_OSS.txe codecov deploy profmrg *.RTDU *.L *.spi *.spl *.dpi *.dyn CODE_COVERAGE.HTML ./CodeCoverage
	-@echo ' '

.PHONY: all clean dependents main-build

-include ../makefile.targets
