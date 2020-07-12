plugins {
    `java-library`
}

dependencies {
    implementation(project(":darklaf-property-loader"))
    implementation(project(":darklaf-utils"))

    compileOnly(project(":darklaf-annotations"))
    annotationProcessor(project(":darklaf-annotations-processor"))

    compileOnly("com.google.auto.service:auto-service-annotations")
    annotationProcessor("com.google.auto.service:auto-service")
}
