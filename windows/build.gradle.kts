plugins {
    java
    `module-info-compile`
    id("dev.nokee.jni-library")
    id("dev.nokee.cpp-language")
    `uber-jni-jar`
    `use-prebuilt-binaries`
}

moduleInfo {
    stubModule("darklaf.core")
}

library {
    dependencies {
        jvmImplementation(projects.darklafNativeUtils)
        jvmImplementation(projects.darklafUtils)
        jvmImplementation(projects.darklafPlatformBase)
        jvmImplementation(projects.darklafTheme)
        jvmImplementation(projects.darklafPropertyLoader)
    }

    targetMachines.addAll(machines.windows.x86, machines.windows.x86_64)
    variants.configureEach {
        resourcePath.set("com/github/weisj/darklaf/platform/${project.name}")
        sharedLibrary {
            compileTasks.configureEach {
                compilerArgs.addAll(
                    toolChain.map {
                        when (it) {
                            is Gcc, is Clang -> listOf(
                                "--std=c++17",
                                "-Wall", "-Wextra", "-pedantic",
                                "-Wno-language-extension-token", "-Wno-ignored-attributes"
                            )
                            is VisualCpp -> listOf("/std:c++17", "/EHsc", "/W4", "/permissive", "/WX")
                            else -> emptyList()
                        }
                    }
                )
                optimizedBinary()
            }
            linkTask.configure {
                linkerArgs.addAll(
                    toolChain.map {
                        when (it) {
                            is Gcc, is Clang -> listOf(
                                "-ldwmapi", "-lGdi32", "-luser32",
                                "-ladvapi32", "-lShell32"
                            )
                            is VisualCpp -> listOf(
                                "dwmapi.lib", "user32.lib", "Gdi32.lib",
                                "Advapi32.lib", "Shell32.lib"
                            )
                            else -> emptyList()
                        }
                    }
                )
            }
        }
    }
}
