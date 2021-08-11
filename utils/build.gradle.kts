plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    compileOnly(libs.nullabilityAnnotations)

    testImplementation(libs.test.junit.api)
    testRuntimeOnly(libs.test.junit.engine)
}

tasks.test {
    useJUnitPlatform()
}
