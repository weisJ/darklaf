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

extensions.configure<CppLibrary> {
    source.from(file("src/main/objectiveCpp"))
}

library {
    targetMachines.addAll(machines.macOS.x86, machines.macOS.x86_64)
}
