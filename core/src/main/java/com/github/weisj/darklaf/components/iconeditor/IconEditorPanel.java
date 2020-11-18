/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.components.iconeditor;

import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import javax.swing.*;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.DynamicUI;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.components.ComponentHelper;
import com.github.weisj.darklaf.components.border.DarkBorders;
import com.github.weisj.darklaf.components.chooser.ListChooser;
import com.github.weisj.darklaf.components.color.QuickColorChooser;
import com.github.weisj.darklaf.components.popup.AttachedPopupComponent;
import com.github.weisj.darklaf.components.popup.SharedComponent;
import com.github.weisj.darklaf.components.renderer.SimpleListCellRenderer;
import com.github.weisj.darklaf.icons.*;
import com.github.weisj.darklaf.layout.LayoutHelper;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.Pair;

public class IconEditorPanel extends JPanel {

    private static final int GAP = 2;

    private final Theme theme;
    private final JLabel renderer;
    private final JPanel valuePanel;
    private final JButton reset;
    private final JButton save;

    private final Map<Object, Object> propertyBackup = new HashMap<>();
    private final Map<Object, Object> properties = new HashMap<>();
    private final Map<Object, Consumer<Object>> updateAction = new HashMap<>();
    private final HashSet<String> changedKeys = new HashSet<>();

    private Dimension pickerSize;
    private Insets spinnerInsets;

    private Icon icon;
    private MutableThemedIcon themedIcon;

    public IconEditorPanel(final Icon icon) {
        this(icon, LafManager.getTheme());
    }

    public IconEditorPanel(final Icon icon, final Theme theme) {
        this.theme = theme;
        setLayout(new BorderLayout());

        valuePanel = new JPanel(new BorderLayout());
        valuePanel.setBorder(LayoutHelper.createEmptyContainerBorder());

        reset = DynamicUI.withLocalizedTooltip(ComponentHelper.createIconOnlyButton(
                DarkUIUtil.ICON_LOADER.getIcon("misc/revert.svg", true),
                DarkUIUtil.ICON_LOADER.getIcon("misc/revertDisabled.svg", true)),
                "Actions.revert");
        reset.addActionListener(e -> restore());

        save = DynamicUI.withLocalizedTooltip(ComponentHelper.createIconOnlyButton(
                DarkUIUtil.ICON_LOADER.getIcon("menu/save.svg", true),
                DarkUIUtil.ICON_LOADER.getIcon("menu/saveDisabled.svg", true)),
                "Actions.save");
        save.addActionListener(e -> save());

        int topBottomPad = save.getPreferredSize().height;
        JPanel rendererPanel = new JPanel(new GridBagLayout());
        renderer = new JLabel();
        rendererPanel.add(renderer);
        rendererPanel.setBackground(IconValues.getIconDefaults(theme).defaults.getColor("background"));
        rendererPanel.setBorder(BorderFactory.createCompoundBorder(
                DarkBorders.createBottomBorderWithSpacing(),
                BorderFactory.createEmptyBorder(topBottomPad, 0, topBottomPad, 0)));

        JPanel holder = new JPanel(new BorderLayout());
        holder.add(LayoutHelper.createPanelWithHoverOverlay(
                rendererPanel,
                LayoutHelper.createHorizontalBox(GAP, save, reset)),
                BorderLayout.NORTH);
        holder.add(valuePanel, BorderLayout.CENTER);
        add(holder, BorderLayout.CENTER);
        setIcon(icon);
        updateButtons();
    }

    public void setIcon(final Icon icon) {
        this.icon = icon;
        if (icon instanceof MutableThemedIcon) {
            themedIcon = (MutableThemedIcon) icon;
            if (themedIcon instanceof CustomThemedIcon) {
                themedIcon = (CustomThemedIcon) ((CustomThemedIcon) themedIcon).copy();
            }
        } else if (icon instanceof DarkSVGIcon) {
            CustomThemedIcon svgIcon = new CustomThemedIcon((DarkSVGIcon) icon);
            svgIcon.setDirectRenderingMode(true);
            themedIcon = svgIcon;
        } else {
            themedIcon = new MutableThemedIconStub(icon);
        }

        themedIcon.setContextProperties(IconValues.getIconDefaults(theme).defaults);
        if (themedIcon instanceof CustomThemedIcon) {
            ((CustomThemedIcon) themedIcon).mergeProperties(CustomThemedIcon.MergeMode.REPLACE_REFERENCES,
                    icon instanceof DarkSVGIcon ? (DarkSVGIcon) icon : null);
        }

        renderer.setIcon(themedIcon);
        updateEditor();
    }

    private void updateEditor() {
        properties.clear();
        propertyBackup.clear();
        updateAction.clear();
        valuePanel.removeAll();
        Map<Object, Object> props = themedIcon.getProperties();
        properties.putAll(props);
        propertyBackup.putAll(props);
        List<String> keys = props.entrySet().stream().sorted((e1, e2) -> {
            boolean color1 = e1.getValue() instanceof Color;
            boolean color2 = e2.getValue() instanceof Color;
            if (color1 && !color2) return -1;
            if (color2 && !color1) return 1;
            return e1.getKey().toString().compareTo(e2.getKey().toString());
        }).map(e -> e.getKey().toString()).collect(Collectors.toList());
        valuePanel.add(LayoutHelper.createTitledColumn(
                keys.stream().map(s -> s + ":").collect(Collectors.toList()),
                keys.stream().map(k -> createValueEditor(k, get(k, Object.class))).collect(Collectors.toList()),
                LayoutHelper.getDefaultSpacing()));
        revalidate();
        repaint();
        SwingUtilities.invokeLater(() -> {
            revalidate();
            repaint();
        });
    }

    private JComponent createValueEditor(final String key, final Object value) {
        if (value instanceof Color) {
            Color color = (Color) value;
            JButton palette = createPaletteButton();
            AttachedPopupComponent.attachChooser(palette,
                    () -> IconValues.getSwatchChooser(theme),
                    c -> {
                        if (c != null) {
                            updateAction.get(key).accept(c);
                            put(key, c);
                        }
                    }, () -> {
                        Pair<Object, Color> entry = getEntry(key, Color.class);
                        return new NamedColor(entry.getFirst().toString(), entry.getSecond());
                    });


            if (pickerSize == null || spinnerInsets == null) {
                pickerSize = palette.getPreferredSize();
                JSpinner sp = new JSpinner(new SpinnerNumberModel(10, 0, 100, 1));
                spinnerInsets = sp.getBorder().getBorderInsets(sp);
                pickerSize.width = sp.getPreferredSize().width - spinnerInsets.left - spinnerInsets.right;
            }

            QuickColorChooser chooser =
                    new QuickColorChooser(null, color, (b, c) -> put(key, c), false, pickerSize);
            chooser.setDragEnabled(true);
            DynamicUI.withLocalizedTooltip(chooser, "Labels.colorChooser");
            chooser.setBorder(LayoutHelper.createEmptyBorder(spinnerInsets));
            updateAction.put(key, o -> {
                if (o instanceof Color) chooser.setColor((Color) o, false);
            });
            return LayoutHelper.createHorizontalBox(GAP, chooser, palette);
        } else if (value instanceof Integer) {
            JButton palette = createPaletteButton();
            AttachedPopupComponent.attachChooser(palette,
                    () -> IconValues.getOpacityChooser(theme),
                    i -> {
                        if (i != null) {
                            updateAction.get(key).accept(i.getValue());
                            put(key, i);
                        }
                    }, () -> {
                        Pair<Object, Integer> entry = getEntry(key, Integer.class);
                        return new NamedInt(entry.getFirst().toString(), entry.getSecond());
                    });

            SpinnerNumberModel numberModel = new SpinnerNumberModel((int) value, 0, 100, 1);
            numberModel.addChangeListener(e -> put(key, numberModel.getValue()));
            updateAction.put(key, o -> {
                if (o instanceof Integer) numberModel.setValue(o);
            });
            return LayoutHelper.createHorizontalBox(GAP, new JSpinner(numberModel), palette);
        } else {
            return new JLabel(value.toString());
        }
    }

    private JButton createPaletteButton() {
        return DynamicUI.withLocalizedTooltip(ComponentHelper.createIconOnlyButton(
                DarkUIUtil.ICON_LOADER.getIcon("misc/palette.svg", true),
                DarkUIUtil.ICON_LOADER.getIcon("misc/paletteDisabled.svg", true)),
                "Actions.predefinedValues");
    }

    private <T> Pair<Object, T> getEntry(final String key, final Class<T> type) {
        return IconColorMapper.getEntry(properties, IconValues.getIconDefaults(theme).defaults, key, null, type);
    }

    private <T> T get(final String key, final Class<T> type) {
        return IconColorMapper.get(properties, IconValues.getIconDefaults(theme).defaults, key, null, type);
    }

    private void put(final String key, final Object value) {
        if (value == null) return;
        Object effectiveValue = value;
        if (value instanceof Named) {
            effectiveValue = PropertyLoader.getReferencePrefix() + ((Named) value).getName();
        }
        Object prevEff = properties.put(key, effectiveValue);
        Object prev = propertyBackup.get(key);
        if (!effectiveValue.equals(prev)) {
            changedKeys.add(key);
        } else {
            changedKeys.remove(key);
        }
        updateButtons();
        if (!effectiveValue.equals(prevEff)) {
            peek();
        }
    }

    /**
     * Get the icon.
     *
     * @return the icon.
     */
    public Icon getIcon() {
        return icon;
    }

    private void peek() {
        themedIcon.setProperties(properties);
        renderer.repaint();
    }

    /**
     * Save the properties.
     */
    public void save() {
        peek();
        propertyBackup.putAll(properties);
        changedKeys.clear();
        updateButtons();
    }

    /**
     * Return the theme of the editor.
     *
     * @return the used theme.
     */
    public Theme getTheme() {
        return theme;
    }

    /**
     * Restore the properties to the last time they have been saved.
     */
    public void restore() {
        themedIcon.setProperties(new HashMap<>(propertyBackup));
        properties.clear();
        properties.putAll(propertyBackup);
        properties.forEach((k, v) -> {
            Consumer<Object> cons = updateAction.get(k);
            if (cons != null) cons.accept(get(k.toString(), Object.class));
        });
        changedKeys.clear();
        renderer.repaint();
        updateButtons();
    }

    /**
     * Get the current properties.
     *
     * @return the current state of the icon properties.
     */
    public Map<String, Object> exportProperties() {
        return properties.entrySet().stream()
                .collect(Collectors.toMap(e -> e.getKey().toString(), Map.Entry::getValue));
    }

    private void updateButtons() {
        boolean changed = !changedKeys.isEmpty();
        reset.getParent().setVisible(changed);
        reset.setEnabled(changed);
        save.setEnabled(changed);
    }

    private static class MutableThemedIconStub extends IconDelegate implements MutableThemedIcon {

        public MutableThemedIconStub(final Icon icon) {
            super(icon);
        }

        @Override
        public Map<Object, Object> getProperties() {
            return Collections.emptyMap();
        }

        @Override
        public void setProperties(final Map<Object, Object> props) {

        }

        @Override
        public void setContextProperties(final Map<Object, Object> props) {

        }

        @Override
        public Map<Object, Object> getContextProperties() {
            return Collections.emptyMap();
        }
    }

    static class IconValues {

        private static final Map<Theme, SharedComponent<ColorPaletteChooser>> chooserMap = new HashMap<>();
        private static final Map<Theme, SharedComponent<OpacityChooser>> opacityChooserMap = new HashMap<>();
        private static final Map<Theme, ThemeIconDefaults> themePropertyMap = new HashMap<>();

        public static ColorPaletteChooser getSwatchChooser(final Theme theme) {
            return chooserMap.computeIfAbsent(theme, t -> new SharedComponent<>(() -> createSwatchChooser(t))).get();
        }

        public static OpacityChooser getOpacityChooser(final Theme theme) {
            return opacityChooserMap
                    .computeIfAbsent(theme, t -> new SharedComponent<>(() -> createOpacityChooser(t))).get();
        }

        public static ThemeIconDefaults getIconDefaults(final Theme theme) {
            return themePropertyMap.computeIfAbsent(theme, t -> {
                UIDefaults defaults;
                if (theme.equals(LafManager.getInstalledTheme())) {
                    defaults = UIManager.getLookAndFeelDefaults();
                } else {
                    defaults = new DarkLaf() {
                        @Override
                        public Theme getTheme() {
                            return t;
                        }
                    }.getDefaults();
                }
                Properties props = new Properties();
                theme.loadIconTheme(props, defaults);
                props.entrySet().forEach(e -> e.setValue(defaults.get(e.getKey())));
                return new ThemeIconDefaults(defaults, props);
            });
        }

        private static ColorPaletteChooser createSwatchChooser(final Theme theme) {
            ThemeIconDefaults defaults = getIconDefaults(theme);
            if (defaults.colors == null) {
                List<Pair<String, Color>> entries = defaults.props.entrySet().stream()
                        .filter(e -> e.getValue() instanceof Color && e.getKey() instanceof String)
                        .map(e -> new Pair<>(e.getKey().toString(), (Color) e.getValue()))
                        .collect(Collectors.toList());
                defaults.colors = entries.stream()
                        .map(p -> new NamedColor(p.getFirst(), p.getSecond()))
                        .collect(Collectors.toList());
            }
            return new ColorPaletteChooser(defaults.colors);
        }

        private static OpacityChooser createOpacityChooser(final Theme theme) {
            ThemeIconDefaults defaults = getIconDefaults(theme);
            if (defaults.opacities == null) {
                List<Pair<String, Integer>> entries = defaults.props.entrySet().stream()
                        .filter(e -> e.getValue() instanceof Integer && e.getKey() instanceof String)
                        .map(e -> new Pair<>(e.getKey().toString(), (Integer) e.getValue()))
                        .collect(Collectors.toList());
                defaults.opacities = entries.stream()
                        .map(p -> new NamedInt(p.getFirst(), p.getSecond()))
                        .collect(Collectors.toList());
            }
            return new OpacityChooser(defaults.opacities);
        }

        public static class ThemeIconDefaults {
            private final UIDefaults defaults;
            private final Properties props;
            private List<NamedColor> colors;
            private List<NamedInt> opacities;

            private ThemeIconDefaults(final UIDefaults defaults, final Properties props) {
                this.props = props;
                this.defaults = defaults;
            }

            public UIDefaults getDefaults() {
                return defaults;
            }
        }
    }

    private static class OpacityChooser extends ListChooser<NamedInt> {

        protected OpacityChooser(final List<NamedInt> values) {
            super(values);
            setBorder(LayoutHelper.createEmptyContainerBorder());
            setCellRenderer(SimpleListCellRenderer.create(v -> v.getValue() + "% " + v.getName()));
        }
    }

    private static class ColorPaletteChooser extends ListChooser<NamedColor> {

        private final SolidColorIcon icon = new SolidColorIcon();

        protected ColorPaletteChooser(final List<NamedColor> values) {
            super(values);
            setBorder(LayoutHelper.createEmptyContainerBorder());
            setCellRenderer(SimpleListCellRenderer.create((c, v) -> {
                icon.setColor(v.getColor());
                c.setIcon(icon);
                c.setText(v.getName());
            }));
        }

    }

}
