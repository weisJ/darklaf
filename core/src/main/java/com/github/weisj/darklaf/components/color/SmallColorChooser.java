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
package com.github.weisj.darklaf.components.color;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import javax.swing.*;

import com.github.weisj.darklaf.color.DarkColorModel;
import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.color.DarkColorModelHSL;
import com.github.weisj.darklaf.color.DarkColorModelRGB;
import com.github.weisj.darklaf.components.DefaultColorPipette;
import com.github.weisj.darklaf.components.border.DarkBorders;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.listener.UpdateDocumentListener;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.ui.colorchooser.ColorPreviewComponent;
import com.github.weisj.darklaf.ui.colorchooser.ColorTriangle;
import com.github.weisj.darklaf.ui.colorchooser.ColorValueFormatter;
import com.github.weisj.darklaf.ui.slider.DarkSliderUI;
import com.github.weisj.darklaf.ui.tabbedpane.DarkTabbedPaneUI;
import com.github.weisj.darklaf.util.ColorUtil;

public class SmallColorChooser extends JPanel {

    private static final DarkColorModel[] COLOR_MODELS = new DarkColorModel[] {DarkColorModelRGB.getInstance(),
            DarkColorModelHSB.getInstance(), DarkColorModelHSL.getInstance()};

    protected ColorTriangle colorTriangle;
    protected ColorPreviewComponent previewComponent;
    protected Color color;
    private Consumer<Color> callback;
    protected boolean valueChanging;
    protected JTabbedPane colorModelTabbedPane;
    protected JFormattedTextField hexField;
    protected Map<DarkColorModel, Runnable> updateMap = new HashMap<>();
    protected ColorValueFormatter hexFormatter;

    public SmallColorChooser(final Color initial, final Consumer<Color> callback) {
        this.color = initial;
        setValueChanging(true);
        setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        add(createTopComponent(), constraints);
        constraints.gridy = 1;
        add(createCenterComponent(), constraints);
        constraints.gridy = 2;
        add(createBottomComponent(), constraints);

        setValueChanging(false);
        initListeners();
        reset(initial, callback);
    }

    public void reset(final Color initial, final Consumer<Color> callback) {
        this.callback = callback;
        colorTriangle.setColor(this, initial);
    }

    protected void initListeners() {
        colorModelTabbedPane.addChangeListener(e -> {
            hexFormatter.setModel(getDarkColorModel());
            colorTriangle.setColorModel(getDarkColorModel());
        });
        colorTriangle.addListener((c, o) -> {
            previewComponent.setColor(c);
            for (DarkColorModel model : COLOR_MODELS) {
                if (o != model) updateMap.get(model).run();
            }
            if (o != hexField) {
                hexField.setText(ColorUtil.toHex(c));
            }
        });
        hexField.getDocument().addDocumentListener((UpdateDocumentListener) () -> {
            try {
                String hexStr = String.format("%1$-" + 8 + "s", hexField.getText()).replaceAll(" ", "F");
                int[] rgb = new int[] {Integer.valueOf(hexStr.substring(0, 2), 16),
                        Integer.valueOf(hexStr.substring(2, 4), 16), Integer.valueOf(hexStr.substring(4, 6), 16)};
                setColor(hexField, DarkColorModelRGB.getInstance(), rgb);
            } catch (NumberFormatException | IndexOutOfBoundsException ignore) {
            }
        });
    }

    protected void setColor(final DarkColorModel source, final int... values) {
        setColor(source, source, values);
    }

    protected void setColor(final Object source, final DarkColorModel model, final int... values) {
        if (isValueChanging()) return;
        setValueChanging(true);
        if (model != null) {
            colorTriangle.setColorFromModel(source, model, values);
        }
        if (callback != null) callback.accept(colorTriangle.getColor());
        setValueChanging(false);
    }

    public boolean isValueChanging() {
        return valueChanging;
    }

    public void setValueChanging(final boolean valueChanging) {
        this.valueChanging = valueChanging;
    }

    protected JComponent createTopComponent() {
        colorTriangle = createColorWheel();
        JPanel holder = new JPanel(new BorderLayout());
        holder.setOpaque(false);
        holder.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        holder.add(colorTriangle, BorderLayout.CENTER);
        return holder;
    }

    protected JComponent createCenterComponent() {
        colorModelTabbedPane = new JTabbedPane() {
            @Override
            public Color getBackground() {
                Component parent = getParent();
                return parent != null ? parent.getBackground() : super.getBackground();
            }
        };
        colorModelTabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_CENTER_TABS, true);
        colorModelTabbedPane.setOpaque(false);
        addColorModels(colorModelTabbedPane, COLOR_MODELS);
        colorModelTabbedPane.setBorder(DarkBorders.createLineBorder(1, 0, 1, 0));
        return colorModelTabbedPane;
    }

    protected JComponent createBottomComponent() {
        Box box = Box.createHorizontalBox();
        box.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
        previewComponent = createPreviewComponent();

        JPanel hexFieldHolder = new JPanel(new GridBagLayout());
        hexFieldHolder.setOpaque(false);
        Box hexBox = Box.createHorizontalBox();
        hexBox.add(new JLabel("#"));
        hexBox.add(createHexField());
        hexFieldHolder.add(hexBox);

        box.add(previewComponent);
        box.add(Box.createGlue());
        box.add(hexFieldHolder);
        box.add(Box.createGlue());
        box.add(createPipetteButton());
        return box;
    }

    private JButton createPipetteButton() {
        Icon pipetteIcon = UIManager.getIcon("ColorChooser.pipette.icon");
        Icon pipetteHoverIcon = UIManager.getIcon("ColorChooser.pipetteRollover.icon");
        JButton pipetteButton = new JButton();
        DefaultColorPipette pipette = new DefaultColorPipette(this, (c, o) -> setColor(pipetteButton,
                DarkColorModelRGB.getInstance(), c.getRed(), c.getGreen(), c.getBlue()));
        pipetteButton.putClientProperty(DarkButtonUI.KEY_THIN, Boolean.TRUE);
        pipetteButton.putClientProperty(DarkButtonUI.KEY_SQUARE, Boolean.TRUE);
        pipetteButton.setRolloverEnabled(true);
        pipetteButton.setFocusable(false);
        pipetteButton.setIcon(pipetteIcon);
        pipetteButton.setRolloverIcon(pipetteHoverIcon);
        pipetteButton.setDisabledIcon(pipetteHoverIcon);
        pipetteButton.setPressedIcon(pipetteHoverIcon);
        pipetteButton.addActionListener(e -> {
            pipetteButton.setEnabled(false);
            pipette.setInitialColor(getColor());
            pipette.show();
        });
        pipette.setCloseAction(() -> pipetteButton.setEnabled(true));
        return pipetteButton;
    }

    protected JComponent createHexField() {
        hexField = new JFormattedTextField();
        hexField.setColumns(6);
        hexField.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT);
        hexFormatter = ColorValueFormatter.init(null, 0, true, hexField);
        hexFormatter.setModel(getDarkColorModel());
        GraphicsUtil.setOpaqueBuffered(hexField, true);
        return hexField;
    }

    public Color getColor() {
        return color;
    }

    public DarkColorModel getDarkColorModel() {
        return COLOR_MODELS[colorModelTabbedPane.getSelectedIndex()];
    }

    protected void addColorModels(final JTabbedPane tabbedPane, final DarkColorModel... colorModels) {
        if (colorModels == null || colorModels.length == 0) {
            throw new IllegalArgumentException("Must pass at least one valid colorModel");
        }
        for (DarkColorModel model : colorModels) {
            tabbedPane.addTab(model.toString(), createColorModelComponent(model));
        }
    }

    protected JComponent createColorModelComponent(final DarkColorModel model) {
        Box box = Box.createVerticalBox();
        box.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        String[] descriptors = model.getFullLabelDescriptorsBefore();
        String[] descriptorsAfter = model.getFullLabelDescriptorsAfter();
        int count = model.getCount();
        JSlider[] sliders = new JSlider[count];
        Descriptor[] labels = new Descriptor[count];
        for (int i = 0; i < count; i++) {
            Descriptor label = new Descriptor(descriptors[i], descriptorsAfter[i]);
            JSlider slider = new JSlider(model.getMinimum(i), model.getMaximum(i));
            slider.putClientProperty(DarkSliderUI.KEY_INSTANT_SCROLL, true);
            slider.setValue(model.getDefault(i));
            slider.setSnapToTicks(false);
            slider.setPaintLabels(false);
            label.setLabelFor(slider);

            label.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
            JPanel holder = new JPanel(new BorderLayout());
            holder.setOpaque(false);
            holder.add(label, BorderLayout.BEFORE_FIRST_LINE);
            holder.add(slider, BorderLayout.CENTER);
            box.add(holder);

            sliders[i] = slider;
            labels[i] = label;

            label.setValue(String.valueOf(slider.getValue()));
            slider.addChangeListener(e -> {
                if (isValueChanging()) return;
                int[] values = new int[count];
                for (int j = 0; j < count; j++) {
                    values[j] = sliders[j].getValue();
                }
                label.setValue(String.valueOf(slider.getValue()));
                setColor(model, values);
            });
        }
        updateMap.put(model, () -> {
            int[] values = colorTriangle.getValuesForModel(model);
            for (int i = 0; i < count; i++) {
                sliders[i].setValue(values[i]);
                labels[i].setValue(String.valueOf(values[i]));
            }
        });
        return box;
    }

    protected ColorTriangle createColorWheel() {
        ColorTriangle wheel = new ColorTriangle();
        wheel.setMinimumSize(150);
        wheel.setOpaque(false);
        return wheel;
    }

    protected ColorPreviewComponent createPreviewComponent() {
        return new ColorPreviewComponent() {
            @Override
            public Dimension getMaximumSize() {
                return getPreferredSize();
            }

            @SuppressWarnings("SuspiciousNameCombination")
            @Override
            public Dimension getPreferredSize() {
                Dimension size = hexField.getPreferredSize();
                size.width = size.height - 2 * UIManager.getInt("TextField.borderThickness");
                size.height = size.width;
                return size;
            }
        };
    }

    protected static class Descriptor extends JLabel {

        protected final String before;
        protected final String after;
        protected String value;

        public Descriptor(final String before, final String after) {
            this.before = before;
            this.after = after;
            setValue(null);
        }

        public void setValue(final String value) {
            this.value = value;
            if (this.value == null) this.value = "";
            setText(before + ": " + value + after);
        }
    }
}
