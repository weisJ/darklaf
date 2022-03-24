plugins {
    java
    `module-info-compile`
    id("dev.nokee.jni-library")
    id("dev.nokee.objective-cpp-language")
    `uber-jni-jar`
    `use-prebuilt-binaries`
    `apple-m1-toolchain`
}

moduleInfo {
    stubModule("darklaf.core")
    stubModule("darklaf.platform.preferences")
    stubModule("darklaf.platform.decorations")
}

val nativeResourcePath = "com/github/weisj/darklaf/platform/${project.name}"

library {
    dependencies {
        jvmImplementation(projects.darklafThemeSpec)
        jvmImplementation(projects.darklafUtils)
        jvmImplementation(projects.darklafNativeUtils)
        jvmImplementation(projects.darklafPlatformBase)
        nativeLibImplementation(macOsFrameworks.appKit)
        nativeLibImplementation(macOsFrameworks.cocoa)
    }

    targetMachines.addAll(machines.macOS.x86_64, machines.macOS.architecture("arm64"))
    variants.configureEach {
        resourcePath.set(nativeResourcePath)
        sharedLibrary {
            val isArm = targetMachine.architectureString == "arm64"
            val minOs = if (isArm) "11" else "10.10"
            compileTasks.configureEach {
                compilerArgs.addAll("-mmacosx-version-min=$minOs")
                // Build type not modeled yet, assuming release
                optimizedBinary()
            }
            linkTask.configure {
                linkerArgs.addAll(
                    "-lobjc", "-mmacosx-version-min=$minOs",
                    // "-framework", "AppKit",
                    // "-framework", "Cocoa",
                )
            }
        }
    }
}
