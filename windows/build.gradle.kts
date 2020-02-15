plugins {
    `jni-library`
}

fun DependencyHandlerScope.javaImplementation(dep: Any) {
    compileOnly(dep)
    runtimeOnly(dep)
}

dependencies {
    javaImplementation(project(":darklaf-native-utils"))
    javaImplementation("net.java.dev.jna:jna")
}

library {
    targetMachines.addAll(machines.windows.x86, machines.windows.x86_64)
    binaries.configureEach {
        compileTask.get().compilerArgs.addAll(toolChain.let {
            if (it is Gcc || it is Clang) listOf("--std=c++11")
            else emptyList()
        })
    }
    binaries.whenElementFinalized(CppSharedLibrary::class) {
        linkTask.get().linkerArgs.addAll(
            "dwmapi.lib",
            "user32.lib",
            "Gdi32.lib",
            "-ldwmapi",
            "-lGdi32",
            "-luser32"
        )
    }
}
