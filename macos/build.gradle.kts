

plugins {
    java
    `module-info-compile`
    id("dev.nokee.jni-library")
    id("dev.nokee.objective-cpp-language")
    `uber-jni-jar`
    `use-prebuilt-binaries`
    `apple-m1-toolchain`
}

val jnfConfig: Configuration by configurations.creating {
    attributes {
        attribute(Attribute.of("dev.nokee.architecture", String::class.java), "arm64")
    }
}

dependencies {
    jnfConfig(macOsFrameworks.javaNativeFoundation)
}

val nativeResourcePath = "com/github/weisj/darklaf/platform/${project.name}"

tasks.jar {
    jnfConfig.asFileTree.forEach {
        from(zipTree(it)) {
            into("$nativeResourcePath/JavaNativeFoundation.framework")
            include("JavaNativeFoundation*")
            exclude("**/*.tbd")
        }
    }
}

library {
    dependencies {
        jvmImplementation(projects.darklafTheme)
        jvmImplementation(projects.darklafNativeUtils)
        jvmImplementation(projects.darklafUtils)
        jvmImplementation(projects.darklafPlatformBase)
        jvmImplementation(projects.darklafPropertyLoader)
        nativeLibImplementation(macOsFrameworks.appKit)
        nativeLibImplementation(macOsFrameworks.cocoa)
        nativeLibImplementation(macOsFrameworks.javaNativeFoundation)
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
                val systemFrameworks = "/System/Library/Frameworks"
                val versionCurrent = "Versions/Current"
                val versionA = "Versions/A"
                val jnfName = "JavaNativeFoundation.framework"
                linkerArgs.addAll(
                    "-lobjc", "-mmacosx-version-min=$minOs",
                    // "-framework", "AppKit",
                    // "-framework", "Cocoa",
                    // The custom JNF framework specified @rpath for searching. As we aren't actually linking
                    // with the dynamic library of the framework we specifically have to add the system framework
                    // search paths accordingly.
                    // First try any system provided framework (this will fail on arm64):
                    "-rpath", "$systemFrameworks/$jnfName/$versionCurrent",
//                    "-rpath", "$systemFrameworks/$jnfName/$versionA",
//                    "-rpath", "$systemFrameworks/JavaVM.framework/$versionCurrent/Frameworks/$jnfName/$versionCurrent",
//                    "-rpath", "$systemFrameworks/JavaVM.framework/$versionA/Frameworks/$jnfName/$versionA",
                    // Then try the jdk provided framework (folder layout may vary. We check multiple possibilities):
//                    "-rpath", "@executable_path/../lib/$jnfName",
//                    "-rpath", "@executable_path/../lib/$jnfName/$versionCurrent",
//                    "-rpath", "@executable_path/../lib/$jnfName/$versionA",
                    // Lastly use our bundled drop-in replacement:
//                    "-rpath", "@loader_path/$jnfName"
                )
            }
        }
    }
}
