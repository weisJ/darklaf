plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    compileOnly(libs.nullabilityAnnotations)
    compileOnly(libs.tools.errorprone.annotations)

    testImplementation(libs.test.junit.api)
    testRuntimeOnly(libs.test.junit.engine)
}

tasks.test {
    useJUnitPlatform()
}
