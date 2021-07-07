import com.github.vlsi.gradle.properties.dsl.stringProperty

plugins {
    java
    id("dev.nokee.jni-library")
    id("dev.nokee.objective-cpp-language")
    `uber-jni-jar`
    `use-prebuilt-binaries`
    `apple-m1-toolchain`
}

repositories {
    maven {
        name = "GitHubPackages"
        url = uri("https://maven.pkg.github.com/weisj/JavaNativeFoundation")
        credentials {
            username = project.stringProperty("gpr.user") ?: System.getenv("GITHUB_ACTOR")
            password = project.stringProperty("gpr.key") ?: System.getenv("GITHUB_TOKEN")
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
        // nativeLibImplementation(libs.macos.appKit)
        // nativeLibImplementation(libs.macos.cocoa)
        nativeLibImplementation(libs.macos.javaNativeFoundation)
    }

    targetMachines.addAll(machines.macOS.x86_64, machines.macOS.architecture("arm64"))
    variants.configureEach {
        resourcePath.set("com/github/weisj/darklaf/platform/${project.name}/${targetMachine.variantName}")
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
                    "-framework", "AppKit",
                    "-framework", "Cocoa",
                    // The custom JNF framework specified @rpath for searching. As we aren't actually linking
                    // with the dynamic library of the framework we specifically have to add the system framework
                    // search paths accordingly.
                    "-rpath", "/System/Library/Frameworks/JavaVM.framework/Versions/A/Frameworks",
                    "-rpath", "/System/Library/Frameworks",
                    "-rpath", "@executable_path/../lib"
                )
            }
        }
    }
}
