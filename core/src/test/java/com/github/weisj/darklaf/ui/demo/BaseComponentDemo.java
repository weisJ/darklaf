/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.github.weisj.darklaf.ui.demo;

import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;

import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.util.LazyValue;

public abstract class BaseComponentDemo implements ComponentDemo, DemoExecutionSpec {

    private static final boolean DEBUG = false;
    private final List<DemoSpec<?>> demoSpecList = new ArrayList<>();
    private final LazyValue<JComponent> componentLazyValue = new LazyValue<>(this::createComponent);

    protected void init() {}

    protected final void addSpec(final DemoSpec<?> spec) {
        demoSpecList.add(spec);
    }

    protected abstract JComponent createComponent();

    @Override
    public JComponent getComponent() {
        return componentLazyValue.get();
    }

    @Override
    public JComponent getContentPane() {
        JComponent component = getComponent();
        if (!supportsSpec()) return component;
        init();

        DemoPanel demoPanel = new DemoPanel(component);
        if (!demoSpecList.isEmpty()) {
            setupControls(component, demoPanel);
        }

        return demoPanel;
    }

    @SuppressWarnings("unchecked")
    private void setupControls(final JComponent component, final DemoPanel demoPanel) {
        final Runnable updateComponent = () -> {
            if (!DEBUG) {
                component.invalidate();
                component.doLayout();
                Component parent = component.getParent();
                if (parent != null) {
                    parent.invalidate();
                    parent.doLayout();
                }
            }
        };

        JComponent controls = demoPanel.addControls();
        for (DemoSpec<?> demoSpec : demoSpecList) {
            if (demoSpec instanceof SpacerSpec) {
                if (controls.getComponentCount() > 0) {
                    controls = demoPanel.addControls(2);
                }
            } else if (demoSpec instanceof BooleanDemoSpec) {
                JCheckBox checkBox = new JCheckBox(demoSpec.getName());
                checkBox.setSelected(((BooleanDemoSpec) demoSpec).get(component) == Boolean.TRUE);
                checkBox.addActionListener(e -> {
                    ((BooleanDemoSpec) demoSpec).set(component, checkBox.isSelected());
                    updateComponent.run();
                });
                controls.add(checkBox);
            } else if (demoSpec instanceof CustomControlSpec) {
                if (controls.getComponentCount() > 0) {
                    controls = demoPanel.addControls();
                }
                ((CustomControlSpec) demoSpec).builderClosure.accept(controls);
                controls = demoPanel.addControls();
            } else {
                JComboBox<Object> comboBox = new JComboBox<>(demoSpec.getPossibleValues().toArray());
                Object initial = demoSpec.get(component);
                if (!demoSpec.getPossibleValues().contains(initial)) {
                    initial = demoSpec.getPossibleValues().get(0);
                }
                comboBox.addItemListener(e -> {
                    ((DemoSpec<Object>) demoSpec).set(component, e.getItem());
                    updateComponent.run();
                });
                comboBox.setSelectedItem(initial);
                controls.add(new JLabel(demoSpec.getName()));
                controls.add(comboBox);
            }
        }
    }

    protected final void booleanSpec(final String key) {
        addSpec(new BooleanDemoSpec(key, new ClientPropertySpecBinding<>(key)));
    }

    protected final <C extends JComponent> void booleanSpec(final String name,
            final BiConsumer<C, Boolean> setter, final Function<C, Boolean> getter) {
        addSpec(new BooleanDemoSpec(name, new FunctionBinding<>(setter, getter)));
    }

    @SuppressWarnings("unchecked")
    protected final <C extends JComponent, T> void binaryOptionSpec(final String name,
            final BiConsumer<C, T> setter, final Function<C, T> getter,
            final T option1, final T option2) {
        booleanSpec(name,
                (c, b) -> setter.accept((C) c, b ? option1 : option2),
                c -> !Objects.equals(getter.apply((C) c), option2));
    }

    protected final void customControls(final Consumer<Container> scope) {
        addSpec(new CustomControlSpec(scope));
    }

    protected final <T extends Enum<T>> void enumSpec(final String key, final Class<T> enumType) {
        addSpec(new EnumDemoSpec<>(key, enumType, new ClientPropertySpecBinding<>(key)));
    }

    protected final <T extends Enum<T>> void enumSpecWithNone(final String key, final Class<T> enumType) {
        List<Object> values = new ArrayList<>();
        values.add(new StringBuffer("none"));
        Collections.addAll(values, enumType.getEnumConstants());
        spec(key, values);
    }

    protected final <T> void spec(final String key, final List<T> options) {
        addSpec(new DemoSpec<>(key, options, new ClientPropertySpecBinding<>(key)));
    }

    @SafeVarargs
    protected final <T> void spec(final String key, final T... options) {
        spec(key, Arrays.asList(options));
    }

    protected final <C extends JComponent, T> void spec(final String name,
            final BiConsumer<C, T> setter, final Function<C, T> getter, final List<T> options) {
        addSpec(new DemoSpec<>(name, options, new FunctionBinding<>(setter, getter)));
    }

    protected final void spacer() {
        addSpec(new SpacerSpec());
    }

    protected boolean supportsSpec() {
        return false;
    }

    @Override
    public List<DemoSpec<?>> getSpecs() {
        return demoSpecList.stream()
                .filter(it -> !(it instanceof SpacerSpec || it instanceof CustomControlSpec))
                .collect(Collectors.toList());
    }

    @Override
    public DemoExecutionSpec getExecutionSpec() {
        return this;
    }

    @SuppressWarnings("unchecked")
    private static class FunctionBinding<T, C extends JComponent> implements SpecBinding<T> {

        private final BiConsumer<C, T> setter;
        private final Function<C, T> getter;

        private FunctionBinding(final BiConsumer<C, T> setter, final Function<C, T> getter) {
            this.setter = setter;
            this.getter = getter;
        }

        @Override
        public T get(final JComponent component) {
            return getter.apply((C) component);
        }

        @Override
        public void set(final JComponent component, final T value) {
            setter.accept((C) component, value);
        }
    }

    private static class CustomControlSpec extends DemoSpec<Void> {

        private final Consumer<Container> builderClosure;

        public CustomControlSpec(final Consumer<Container> builderClosure) {
            super("", Collections.emptyList(), null);
            this.builderClosure = builderClosure;
        }
    }
}
