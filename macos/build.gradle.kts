import org.gradle.util.VersionNumber

plugins {
    java
    id("dev.nokee.jni-library")
    id("dev.nokee.objective-cpp-language")
    `uber-jni-jar`
    `use-prebuilt-binaries`
}

library {
    dependencies {
        jvmImplementation(projects.darklafTheme)
        jvmImplementation(projects.darklafNativeUtils)
        jvmImplementation(projects.darklafUtils)
        jvmImplementation(projects.darklafPlatformBase)
        jvmImplementation(projects.darklafPropertyLoader)
        nativeLibImplementation(libs.macos.appKit)
        nativeLibImplementation(libs.macos.cocoa)
        val xCodeVersion = getXCodeVersion()
        if (xCodeVersion != null && xCodeVersion >= VersionNumber.parse("12.2")) {
            nativeLibImplementation(libs.macos.javaNativeFoundation)
        } else {
            nativeLibImplementation(libs.macosLegacy.javaVM.base)
            nativeLibImplementation(libs.macosLegacy.javaVM.base) {
                capabilities {
                    requireLibCapability(libs.macosLegacy.javaVM.capability.javaNativeFoundation)
                }
            }
        }
    }

    targetMachines.addAll(machines.macOS.x86_64)
    variants.configureEach {
        resourcePath.set("com/github/weisj/darklaf/platform/${project.name}/${targetMachine.variantName}")
        sharedLibrary {
            val minOs = "10.10"
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
