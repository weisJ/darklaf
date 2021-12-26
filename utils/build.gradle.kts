plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    compileOnly(libs.nullabilityAnnotations)
    compileOnly(toolLibs.errorprone.annotations)

    testImplementation(testLibs.junit.api)
    testRuntimeOnly(testLibs.junit.engine)
}

tasks.test {
    useJUnitPlatform()
}
