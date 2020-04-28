import UberJniJarPlugin.asVariantName

plugins {
    java
    id("dev.nokee.jni-library")
    id("dev.nokee.cpp-language")
    `uber-jni-jar`
}

library {
    dependencies {
        jvmImplementation(project(":darklaf-native-utils"))
        jvmImplementation(project(":darklaf-utils"))
        jvmImplementation(project(":darklaf-platform-base"))
        jvmImplementation(project(":darklaf-theme"))
        jvmImplementation(project(":darklaf-property-loader"))
        jvmImplementation("net.java.dev.jna:jna")
    }
    targetMachines.addAll(machines.windows.x86, machines.windows.x86_64)
    variants.configureEach {
        resourcePath.set("com/github/weisj/darklaf/platform/${project.name}/${asVariantName(targetMachine)}")
        sharedLibrary {
            compileTasks.configureEach {
                compilerArgs.addAll(toolChain.map {
                    when (it) {
                        is Gcc, is Clang -> listOf("--std=c++11")
                        is VisualCpp -> listOf("/EHsc")
                        else -> emptyList()
                    }
                })

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
                linkerArgs.addAll(toolChain.map {
                    when (it) {
                        is Gcc, is Clang -> listOf("-ldwmapi", "-lGdi32", "-luser32", "-ladvapi32", "-Shell32")
                        is VisualCpp -> listOf("dwmapi.lib", "user32.lib", "Gdi32.lib", "Advapi32.lib", "Shell32.lib")
                        else -> emptyList()
                    }
                })
            }
        }
    }
}
