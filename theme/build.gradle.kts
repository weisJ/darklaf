plugins {
    `java-library`
}

dependencies {
    implementation(project(":darklaf-property-loader"))
    implementation(project(":darklaf-utils"))

    compileOnly(project(":darklaf-annotations"))
    annotationProcessor(project(":darklaf-annotations-processor"))

    annotationProcessor("com.google.auto.service:auto-service")
    compileOnly("com.google.auto.service:auto-service-annotations")
}
