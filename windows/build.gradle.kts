

plugins {
    `jni-library`
}

fun DependencyHandlerScope.javaImplementation(dep: Any) {
    compileOnly(dep)
    runtimeOnly(dep)
}

dependencies {
    javaImplementation(project(":darklaf-native-utils"))
    javaImplementation(project(":darklaf-utils"))
    javaImplementation(project(":darklaf-platform-base"))
    javaImplementation(project(":darklaf-theme"))
    javaImplementation(project(":darklaf-property-loader"))
    javaImplementation("net.java.dev.jna:jna")
}

library {
    targetMachines.addAll(machines.windows.x86, machines.windows.x86_64)
    binaries.configureEach {
        compileTask.get().compilerArgs.addAll(
            when (toolChain) {
                is Gcc, is Clang -> listOf("--std=c++11")
                is VisualCpp -> listOf("/EHsc")
                else -> emptyList()
            }
        )
    }
    binaries.whenElementFinalized(CppSharedLibrary::class) {
        linkTask.get().linkerArgs.addAll(
            when (toolChain) {
                is Gcc, is Clang -> listOf("-ldwmapi", "-lGdi32", "-luser32", "-ladvapi32")
                is VisualCpp -> listOf("dwmapi.lib", "user32.lib", "Gdi32.lib", "Advapi32.lib")
                else -> emptyList()
            }
        )
    }
}
