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
    javaImplementation(project(":darklaf-decorations-base"))
    javaImplementation(project(":darklaf-property-loader"))
}

library {
    targetMachines.addAll(machines.macOS.x86_64)
    binaries.configureEach {
        compileTask.get().compilerArgs.addAll(
            listOf("-x", "objective-c++")
        )
    }
    binaries.whenElementFinalized(CppSharedLibrary::class) {
        linkTask.get().linkerArgs.addAll(listOf("-lobjc"))
    }
}
