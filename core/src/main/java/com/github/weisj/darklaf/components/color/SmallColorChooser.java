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
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.text.*;

import com.github.weisj.darklaf.color.DarkColorModel;
import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.color.DarkColorModelHSL;
import com.github.weisj.darklaf.color.DarkColorModelRGB;
import com.github.weisj.darklaf.components.DefaultColorPipette;
import com.github.weisj.darklaf.components.border.DarkBorders;
import com.github.weisj.darklaf.components.border.MarginBorderWrapper;
import com.github.weisj.darklaf.components.chooser.ChooserComponent;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.layout.LayoutHelper;
import com.github.weisj.darklaf.listener.UpdateDocumentListener;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.ui.colorchooser.ColorPipette;
import com.github.weisj.darklaf.ui.colorchooser.ColorPreviewComponent;
import com.github.weisj.darklaf.ui.colorchooser.ColorTriangle;
import com.github.weisj.darklaf.ui.colorchooser.ColorValueFormatter;
import com.github.weisj.darklaf.ui.slider.DarkSliderUI;
import com.github.weisj.darklaf.ui.tabbedpane.DarkTabbedPaneUI;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class SmallColorChooser extends JPanel implements ChooserComponent<Color> {

    private static final DarkColorModel[] COLOR_MODELS = new DarkColorModel[] {DarkColorModelRGB.getInstance(),
            DarkColorModelHSB.getInstance(), DarkColorModelHSL.getInstance()};

    protected ColorTriangle colorTriangle;
    protected ColorPreviewComponent previewComponent;
    private Consumer<Color> callback;
    private Color initial;

    protected boolean valueChanging;
    protected JTabbedPane colorModelTabbedPane;
    protected JFormattedTextField hexField;
    protected Map<DarkColorModel, Runnable> updateMap = new HashMap<>();
    protected ColorValueFormatter hexFormatter;
    protected ColorPipette pipette;

    public SmallColorChooser() {
        this(Color.BLACK, c -> {
        });
    }

    public SmallColorChooser(final Color initial, final Consumer<Color> callback) {
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

        setOpaque(false);
        setValueChanging(false);
        initListeners();
        reset(initial, callback);
    }

    @Override
    public void reset(final Color initial, final Consumer<Color> callback) {
        this.callback = callback;
        this.initial = initial;
        colorTriangle.setColor(this, initial);
    }

    @Override
    public Color getInitial() {
        return initial;
    }

    @Override
    public Color getSelected() {
        return getColor();
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
        if (callback != null && isShowing()) callback.accept(colorTriangle.getColor());
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
        JLabel label = new JLabel("#");
        hexBox.add(label);
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
        if (pipetteIcon == null) pipetteIcon = DarkUIUtil.ICON_LOADER.getIcon("misc/pipette.svg", true);
        Icon pipetteHoverIcon = UIManager.getIcon("ColorChooser.pipetteRollover.icon");
        JButton pipetteButton = new JButton();
        DefaultColorPipette defaultPipette = new DefaultColorPipette(this, (c, o) -> setColor(pipetteButton,
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
        defaultPipette.setCloseAction(() -> pipetteButton.setEnabled(true));
        pipette = defaultPipette;
        return pipetteButton;
    }

    protected JComponent createHexField() {
        hexField = new JFormattedTextField();
        hexField.setColumns(6);
        hexField.setMargin(new Insets(2, 2, 2, 2));
        hexField.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT);
        hexFormatter = ColorValueFormatter.init(null, 0, true, hexField);
        hexFormatter.setModel(getDarkColorModel());
        GraphicsUtil.setOpaqueBuffered(hexField, true);
        return hexField;
    }

    public Color getColor() {
        return colorTriangle.getColor();
    }

    public DarkColorModel getDarkColorModel() {
        return COLOR_MODELS[colorModelTabbedPane.getSelectedIndex()];
    }

    public boolean isPipetteShowing() {
        return pipette != null && pipette.isShowing();
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
        box.setBorder(LayoutHelper.createEmptyContainerBorder());
        String[] descriptors = model.getFullLabelDescriptorsBefore();
        String[] descriptorsAfter = model.getFullLabelDescriptorsAfter();
        int count = model.getCount();
        JSlider[] sliders = new JSlider[count];
        Descriptor[] labels = new Descriptor[count];
        for (int i = 0; i < count; i++) {
            JSlider slider = new JSlider(model.getMinimum(i), model.getMaximum(i));
            slider.putClientProperty(DarkSliderUI.KEY_INSTANT_SCROLL, true);
            slider.setValue(model.getDefault(i));
            slider.setSnapToTicks(false);
            slider.setPaintLabels(false);
            slider.setOpaque(false);

            Descriptor label = new Descriptor(descriptors[i], descriptorsAfter[i],
                    model.getMinimum(i), model.getMaximum(i), slider::setValue);
            label.setLabelFor(slider);

            label.setBorder(BorderFactory.createEmptyBorder(0, LayoutHelper.getDefaultSpacing(), 0, 0));
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

            @Override
            public Dimension getPreferredSize() {
                Dimension dim = hexField.getPreferredSize();
                Border border = MarginBorderWrapper.getBorder(hexField);
                Insets ins = border != null ? border.getBorderInsets(hexField) : null;
                int size = dim.height;
                if (ins != null) size -= ins.top + ins.bottom;
                dim.width = size;
                dim.height = size;
                return dim;
            }
        };
    }

    protected static class Descriptor extends JPanel {


        private final JFormattedTextField textField;
        private final JLabel label;

        public Descriptor(final String before, final String after, final int min, final int max,
                final Consumer<Integer> callback) {
            setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
            setAlignmentX(Component.LEFT_ALIGNMENT);
            label = new JLabel(before + ":");
            add(label);
            textField = new JFormattedTextField(new JFormattedTextField.AbstractFormatterFactory() {
                @Override
                public JFormattedTextField.AbstractFormatter getFormatter(final JFormattedTextField tf) {
                    return new JFormattedTextField.AbstractFormatter() {

                        @Override
                        public void install(final JFormattedTextField ftf) {
                            super.install(ftf);
                            ftf.setCaretPosition(ftf.getCaretPosition());
                        }

                        private final DocumentFilter documentFilter = new DocumentFilter() {
                            @Override
                            public void insertString(final FilterBypass fb, final int offset, final String string,
                                    final AttributeSet attr)
                                    throws BadLocationException {
                                if (!isValidString(string)) return;
                                super.insertString(fb, offset, string, attr);
                            }

                            @Override
                            public void replace(final FilterBypass fb, final int offset, final int length,
                                    final String text,
                                    final AttributeSet attrs)
                                    throws BadLocationException {
                                if (!isValidString(text)) return;
                                super.replace(fb, offset, length, text, attrs);
                            }

                            private boolean isValidString(final String text) {
                                for (int i = 0; i < text.length(); i++) {
                                    char c = text.charAt(i);
                                    if (c < '0' || c > '9') return false;
                                }
                                return true;
                            }
                        };

                        private final NavigationFilter navigationFilter = new NavigationFilter() {
                            @Override
                            public void setDot(final FilterBypass fb, final int dot, final Position.Bias bias) {
                                super.setDot(fb, clampDot(dot, tf), bias);
                            }

                            @Override
                            public void moveDot(final FilterBypass fb, final int dot, final Position.Bias bias) {
                                super.moveDot(fb, clampDot(dot, tf), bias);
                            }

                            @Override
                            public int getNextVisualPositionFrom(final JTextComponent text, final int pos,
                                    final Position.Bias bias,
                                    final int direction,
                                    final Position.Bias[] biasRet) throws BadLocationException {
                                return clampDot(super.getNextVisualPositionFrom(text, pos, bias, direction, biasRet),
                                        text);
                            }

                            private int clampDot(final int dot, final JTextComponent c) {
                                return Math.max(0, Math.min(dot, c.getDocument().getLength() - after.length()));
                            }
                        };

                        @Override
                        public Object stringToValue(final String text) throws ParseException {
                            int value = Integer.parseInt(text.substring(0, text.length() - after.length()));
                            if (value < min || value > max) throw new ParseException(text, 0);
                            return value;
                        }

                        @Override
                        public String valueToString(final Object value) {
                            return value + after;
                        }

                        @Override
                        protected DocumentFilter getDocumentFilter() {
                            return documentFilter;
                        }

                        @Override
                        protected NavigationFilter getNavigationFilter() {
                            return navigationFilter;
                        }
                    };
                }
            });
            AtomicBoolean adjusting = new AtomicBoolean();
            textField.addPropertyChangeListener("value", e -> {
                if (adjusting.get()) return;
                adjusting.set(true);
                if (textField.getValue() instanceof Integer) {
                    callback.accept((Integer) textField.getValue());
                }
                adjusting.set(false);
            });
            textField.setBorder(BorderFactory.createEmptyBorder());
            textField.setOpaque(false);
            setOpaque(false);
            add(textField);
            setValue(min);
        }

        public void setValue(final Object value) {
            textField.setValue(value);
        }

        public void setLabelFor(final JComponent c) {
            label.setLabelFor(c);
        }
    }
}
