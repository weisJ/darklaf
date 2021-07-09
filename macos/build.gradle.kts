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

val jnfConfig: Configuration by configurations.creating {
    attributes {
        attribute(Attribute.of("dev.nokee.architecture", String::class.java), "arm64")
    }
}

dependencies {
    jnfConfig("com.github.weisj:java-native-foundation:1.0.0")
}

val nativeResourcePath = "com/github/weisj/darklaf/platform/${project.name}"

tasks.jar {
    jnfConfig.asFileTree.forEach {
        from(zipTree(it)) {
            into("$nativeResourcePath/JavaNativeFoundation.framework")
            include("Versions/A/JavaNativeFoundation*")
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
        nativeLibImplementation(libs.macos.appKit)
        nativeLibImplementation(libs.macos.cocoa)
        nativeLibImplementation(libs.macos.javaNativeFoundation)
    }

    targetMachines.addAll(machines.macOS.x86_64, machines.macOS.architecture("arm64"))
    variants.configureEach {
        resourcePath.set("$nativeResourcePath/${targetMachine.variantName}")
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
                    // The custom JNF framework specified @rpath for searching. As we aren't actually linking
                    // with the dynamic library of the framework we specifically have to add the system framework
                    // search paths accordingly.
                    // First try any system provided framework (this will fail on arm64):
                    "-rpath", "/System/Library/Frameworks",
                    "-rpath", "/System/Library/Frameworks/JavaVM.framework/Versions/A/Frameworks",
                    // Then try the jdk provided framework:
                    "-rpath", "@executable_path/../lib",
                    // Lastly use our bundled drop-in replacement:
                    "-rpath", "@loader_path"
                )
            }
        }
    }
}
