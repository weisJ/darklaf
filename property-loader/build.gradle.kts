plugins {
    `java-library`
}

dependencies {
    api(project(":darklaf-utils"))
    implementation("com.formdev:svgSalamander")
    testImplementation("org.junit.jupiter:junit-jupiter-api")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine")
}

tasks.test {
    useJUnitPlatform()
}
