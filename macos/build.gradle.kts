import JniUtils.asVariantName

plugins {
    java
    id("dev.nokee.jni-library")
    id("dev.nokee.objective-cpp-language")
    `uber-jni-jar`
    `use-prebuilt-binaries`
}

library {
    val minOs = "10.10"
    val frameworkVersion = "10.15"

    dependencies {
        jvmImplementation(project(":darklaf-theme"))
        jvmImplementation(project(":darklaf-native-utils"))
        jvmImplementation(project(":darklaf-utils"))
        jvmImplementation(project(":darklaf-platform-base"))
        jvmImplementation(project(":darklaf-property-loader"))
        nativeImplementation("dev.nokee.framework:JavaVM:[$frameworkVersion,)")
        nativeImplementation("dev.nokee.framework:JavaVM:[$frameworkVersion,)") {
            capabilities {
                requireCapability("JavaVM:JavaNativeFoundation:[$frameworkVersion,)")
            }
        }
        nativeImplementation("dev.nokee.framework:AppKit:[$frameworkVersion,)")
        nativeImplementation("dev.nokee.framework:Cocoa:[$frameworkVersion,)")
    }

    targetMachines.addAll(machines.macOS.x86_64)
    variants.configureEach {
        resourcePath.set("com/github/weisj/darklaf/platform/${project.name}/${asVariantName(targetMachine)}")
        sharedLibrary {
            compileTasks.configureEach {
                compilerArgs.addAll("-mmacosx-version-min=$minOs")
                // Build type not modeled yet, assuming release
                compilerArgs.addAll(toolChain.map {
                    when (it) {
                        is Gcc, is Clang -> listOf("-O2")
                        is VisualCpp -> listOf("/O2")
                        else -> emptyList()
                    }
                })
            }
            linkTask.configure {
                linkerArgs.addAll("-lobjc", "-mmacosx-version-min=$minOs")
            }
        }
    }
}
