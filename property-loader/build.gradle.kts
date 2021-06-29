plugins {
    `java-library`
    `module-info-compile`
}

dependencies {
    api(projects.darklafUtils)
    implementation(libs.svgSalamander)
    implementation(libs.visualPaddings)
    compileOnly(libs.nullabilityAnnotations)
    testImplementation(libs.test.junit.api)
    testRuntimeOnly(libs.test.junit.engine)
}

tasks.test {
    useJUnitPlatform()
}
