import UberJniJarPlugin.asVariantName

plugins {
    java
    id("dev.nokee.jni-library")
    id("dev.nokee.objective-cpp-language")
    `uber-jni-jar`
}

library {
    dependencies {
        jvmImplementation(project(":darklaf-theme"))
        jvmImplementation(project(":darklaf-native-utils"))
        jvmImplementation(project(":darklaf-utils"))
        jvmImplementation(project(":darklaf-platform-base"))
        jvmImplementation(project(":darklaf-property-loader"))
        nativeImplementation("dev.nokee.framework:JavaVM:10.15")
        nativeImplementation("dev.nokee.framework:JavaVM:10.15") {
            capabilities {
                requireCapability("JavaVM:JavaNativeFoundation:10.15")
            }
        }
    }

    targetMachines.addAll(machines.macOS.x86_64)
    variants.configureEach {
        resourcePath.set("com/github/weisj/darklaf/platform/${project.name}/${asVariantName(targetMachine)}")
        sharedLibrary {
            compileTasks.configureEach {
                compilerArgs.addAll("-mmacosx-version-min=10.10")

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
                linkerArgs.addAll("-lobjc", "-framework", "AppKit")
            }
        }
    }
}
