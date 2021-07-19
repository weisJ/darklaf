package com.github.weisj.darklaf.ui.demo;

import javax.swing.JComponent;
import java.util.Collections;

public final class SpacerSpec extends DemoSpec<Void> {

    public SpacerSpec() {
        super("", Collections.emptyList(), new SpecBinding<Void>() {
            @Override
            public Void get(final JComponent component) {
                return null;
            }

            @Override
            public void set(final JComponent component, final Void value) {}
        });
    }
}
