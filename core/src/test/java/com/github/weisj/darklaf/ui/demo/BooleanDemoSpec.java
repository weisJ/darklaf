package com.github.weisj.darklaf.ui.demo;

import java.util.Arrays;

public class BooleanDemoSpec extends DemoSpec<Boolean> {

    public BooleanDemoSpec(final String name, final SpecBinding<Boolean> binding) {
        super(name, Arrays.asList(true, false), binding);
    }
}
