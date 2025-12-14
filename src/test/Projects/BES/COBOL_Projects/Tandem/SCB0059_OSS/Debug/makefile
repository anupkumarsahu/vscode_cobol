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


BUILD_ARTIFACT_NAME := SCB0059_OSS
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
main-build: SCB0059_OSS.txe

# Tool invocations
SCB0059_OSS.txe: $(OBJS) $(USER_OBJS) makefile $(OPTIONAL_TOOL_DEPS)
	@echo 'Building target: $@'
	@echo 'Invoking: COBOL Linker (TNS/X)'
	xcobol -Wsystype=oss -Wcall_shared -o "SCB0059_OSS.txe" $(OBJS) $(USER_OBJS_WOESC) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '

deploy: SCB0059_OSS.txe makefile $(OPTIONAL_TOOL_DEPS)
	@echo 'Invoking: Deploy Tool'
	java -jar "$(NSDEE_TOOLS_LOC)/Deploy.jar" -q=oss -code=500 -mode=bin -p= -t= -h= -u= -r="SCB0059_OSS.txe" -f="SCB0059_OSS.txe"
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
	-$(RM) SCB0059_OSS.txe codecov deploy profmrg *.RTDU *.L *.spi *.spl *.dpi *.dyn CODE_COVERAGE.HTML ./CodeCoverage
	-@echo ' '

.PHONY: all clean dependents main-build

-include ../makefile.targets
