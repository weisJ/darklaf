/*
 * MIT License
 *
 * Copyright (c) 2019-2024 Jannis Weis
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
package com.github.weisj.darklaf.ui.colorchooser;

import java.awt.*;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import javax.swing.colorchooser.ColorSelectionModel;
import javax.swing.event.AncestorEvent;

import com.github.weisj.darklaf.components.DefaultColorPipette;
import com.github.weisj.darklaf.listener.AncestorAdapter;
import com.github.weisj.darklaf.listener.UpdateDocumentListener;
import com.github.weisj.darklaf.properties.color.DarkColorModel;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author pegov
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkColorChooserPanel extends AbstractColorChooserPanel implements ColorListener {

    private static final Logger LOGGER = LogUtil.getLogger(DarkColorChooserPanel.class);
    public static final String TRANSPARENCY_ENABLED_PROPERTY = "transparency";

    private final Icon pipetteIcon;
    private final Icon pipetteHoverIcon;

    private final ColorPipette pipette;
    private final ColorWheelPanel colorWheelPanel;
    private final ColorPreviewComponent previewComponent;

    private JFormattedTextField[] valueFields;
    private ColorValueFormatter[] formatters;
    private JLabel[] descriptors;
    private JLabel[] descriptorsAfter;
    private JFormattedTextField textHex;
    private ColorValueFormatter hexFormatter;

    private final JComboBox<DarkColorModel> formatBox;

    private Color currentColor;

    protected boolean isChanging;

    public DarkColorChooserPanel(final DarkColorModel... colorModels) {
        if (colorModels == null || colorModels.length == 0) {
            throw new IllegalArgumentException("Must pass at least one valid colorModel");
        }
        isChanging = true;
        previewComponent = new ColorPreviewComponent();
        colorWheelPanel = new ColorWheelPanel(true, true);
        pipette = new DefaultColorPipette(this, colorWheelPanel::setColor);
        pipetteIcon = UIManager.getIcon("ColorChooser.pipette.icon");
        pipetteHoverIcon = UIManager.getIcon("ColorChooser.pipetteRollover.icon");

        formatBox = createColorFormatChooser(colorModels);

        initInputFields(colorModels);

        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(20, 20, 0, 21));
        add(buildTopPanel(UIManager.getBoolean("ColorChooser.pipetteEnabled")), BorderLayout.NORTH);
        add(colorWheelPanel, BorderLayout.CENTER);
        add(Box.createVerticalStrut(10), BorderLayout.SOUTH);

        installListeners();
        // Finalized by #buildChooser
    }

    @Override
    protected void buildChooser() {
        isChanging = false;
        onModelChange();
        colorChanged(getColorFromModel(), this);
    }

    protected void installListeners() {
        formatBox.addActionListener(e -> onModelChange());
        // Make sure the hex field is selected at start.
        textHex.addAncestorListener(new AncestorAdapter() {
            @Override
            public void ancestorAdded(final AncestorEvent event) {
                textHex.requestFocusInWindow();
                textHex.removeAncestorListener(this);
            }
        });
        textHex.getDocument()
                .addDocumentListener((UpdateDocumentListener) () -> colorChanged(getColorFromHex(), textHex));
        for (JFormattedTextField inputField : valueFields) {
            inputField.addPropertyChangeListener(e -> {
                if ("value".equals(e.getPropertyName())) {
                    colorChanged(getColorFromFields(), inputField);
                }
            });
        }
        colorWheelPanel.addListener(this);
    }

    protected Color getColorFromHex() {
        try {
            String hexStr = String.format("%1$-" + 8 + "s", textHex.getText()).replaceAll(" ", "F");
            int alpha = isColorTransparencySelectionEnabled() ? Integer.valueOf(hexStr.substring(6, 8), 16) : 255;
            return new Color(Integer.valueOf(hexStr.substring(0, 2), 16), Integer.valueOf(hexStr.substring(2, 4), 16),
                    Integer.valueOf(hexStr.substring(4, 6), 16), alpha);
        } catch (NumberFormatException | IndexOutOfBoundsException e) {
            LOGGER.log(Level.SEVERE, "Parsing color from hex failed", e);
            return null;
        }
    }

    protected Color getColorFromFields() {
        DarkColorModel model = getDarkColorModel();
        int[] values = new int[model.getCount()];
        for (int i = 0; i < values.length; i++) {
            values[i] = (int) valueFields[i].getValue();
        }
        Color c = model.getColorFromValues(values);
        if (isColorTransparencySelectionEnabled()) {
            c = ColorUtil.toAlpha(c, getColorFromModel().getAlpha());
        }
        return c;
    }

    @Override
    public void colorChanged(final Color color, final Object source) {
        if (isChanging || color == null) return;
        isChanging = true;
        currentColor = color;
        ColorSelectionModel model = getColorSelectionModel();
        if (model != null) model.setSelectedColor(currentColor);
        applyColorToFields(color);
        if (source != textHex) textHex.setValue(color);
        previewComponent.setColor(color);
        colorWheelPanel.setColor(color, this);
        isChanging = false;
    }

    protected void onModelChange() {
        if (isChanging) return;
        isChanging = true;
        colorWheelPanel.setModel(getDarkColorModel());
        updateDescriptors();
        toggleValueFields();
        updateFormatters();
        applyColorToFields(getColorFromModel());
        doLayout();
        isChanging = false;
    }

    protected void applyColorToFields(final Color color) {
        DarkColorModel model = getDarkColorModel();
        int[] values = model.getValuesFromColor(color);
        for (int i = 0; i < values.length; i++) {
            valueFields[i].setValue(values[i]);
        }
    }

    public void initInputFields(final DarkColorModel[] colorModels) {
        int record = getMaxFieldCount(colorModels);
        descriptors = new JLabel[record];
        descriptorsAfter = new JLabel[record];

        textHex = createColorField(true);

        hexFormatter = ColorValueFormatter.init(getDarkColorModel(), 0, true, textHex);
        hexFormatter.setTransparencyEnabled(isColorTransparencySelectionEnabled());

        valueFields = new JFormattedTextField[record];
        formatters = new ColorValueFormatter[record];

        for (int i = 0; i < record; i++) {
            descriptors[i] = new JLabel();
            descriptorsAfter[i] = new JLabel();
            valueFields[i] = createColorField(false);
            formatters[i] = ColorValueFormatter.init(getDarkColorModel(), i, false, valueFields[i]);
        }
    }

    private JFormattedTextField createColorField(final boolean hex) {
        JFormattedTextField field = new JFormattedTextField(0);
        field.setColumns(hex ? 8 : 3);
        field.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT);
        return field;
    }

    protected int getMaxFieldCount(final DarkColorModel[] colorModels) {
        int record = 0;
        for (DarkColorModel model : colorModels) {
            record = Math.max(model.getCount(), record);
        }
        return record;
    }

    protected JComboBox<DarkColorModel> createColorFormatChooser(final DarkColorModel[] colorModels) {
        JComboBox<DarkColorModel> comboBox = new JComboBox<>(colorModels);
        DarkColorModel prototype = null;
        for (DarkColorModel model : colorModels) {
            String name = model.toString();
            if (prototype == null || prototype.toString().length() < name.length()) {
                prototype = model;
            }
        }
        comboBox.setPrototypeDisplayValue(prototype);
        return comboBox;
    }

    private void updateDescriptors() {
        String[] desc = getDarkColorModel().getLabelDescriptorsBefore();
        String[] descAfter = getDarkColorModel().getFullLabelDescriptorsAfter();
        for (int i = 0; i < descriptors.length; i++) {
            if (i < desc.length) {
                descriptors[i].setText(desc[i] + ":");
            } else {
                descriptors[i].setText("");
            }
            if (i < descAfter.length) {
                descriptorsAfter[i].setText(descAfter[i]);
            } else {
                descriptorsAfter[i].setText("");
            }
        }
    }

    private void toggleValueFields() {
        DarkColorModel model = getDarkColorModel();
        int count = model.getCount();
        for (int i = 0; i < valueFields.length; i++) {
            valueFields[i].setEnabled(i < count);
            valueFields[i].setVisible(i < count);
        }
    }

    private void updateFormatters() {
        for (ColorValueFormatter formatter : formatters) {
            formatter.setModel(getDarkColorModel());
        }
    }

    @Override
    protected Color getColorFromModel() {
        Color c = super.getColorFromModel();
        return c == null ? currentColor : c;
    }

    protected DarkColorModel getDarkColorModel() {
        return (DarkColorModel) formatBox.getSelectedItem();
    }

    private JComponent buildTopPanel(final boolean enablePipette) {
        final JPanel result = new JPanel(new BorderLayout());

        final JPanel previewPanel = new JPanel(new BorderLayout());
        if (enablePipette && pipette != null) {
            previewPanel.add(createPipetteButton(), BorderLayout.WEST);
        }
        previewComponent.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
        previewComponent.setPreferredSize(new Dimension(100, 25));

        previewPanel.add(previewComponent, BorderLayout.CENTER);
        result.add(previewPanel, BorderLayout.NORTH);

        final JPanel valuePanel = new JPanel();
        valuePanel.setLayout(new BoxLayout(valuePanel, BoxLayout.X_AXIS));
        valuePanel.setBorder(BorderFactory.createEmptyBorder(10, 0, 0, 0));

        for (int i = 0; i < descriptorsAfter.length; i++) {
            descriptorsAfter[i].setPreferredSize(new Dimension(14, -1));
            descriptors[i].setPreferredSize(new Dimension(14, -1));
            valuePanel.add(descriptors[i]);
            valuePanel.add(valueFields[i]);
            valuePanel.add(descriptorsAfter[i]);
            if (i < descriptorsAfter.length - 1) {
                valuePanel.add(Box.createHorizontalStrut(2));
            }
        }
        result.add(valuePanel, BorderLayout.WEST);

        final JPanel hexPanel = new JPanel();
        hexPanel.setLayout(new BoxLayout(hexPanel, BoxLayout.X_AXIS));
        hexPanel.setBorder(BorderFactory.createEmptyBorder(10, 0, 0, 0));

        hexPanel.add(formatBox);
        hexPanel.add(Box.createHorizontalStrut(2));
        hexPanel.add(new JLabel("#"));
        hexPanel.add(textHex);

        result.add(hexPanel, BorderLayout.EAST);
        return result;
    }

    private JButton createPipetteButton() {
        JButton pipetteButton = new JButton();
        pipetteButton.putClientProperty(DarkButtonUI.KEY_SQUARE, Boolean.TRUE);
        pipetteButton.setRolloverEnabled(true);
        pipetteButton.setFocusable(false);

        pipetteButton.setIcon(getPipetteIcon());
        pipetteButton.setRolloverIcon(getPipetteRolloverIcon());
        pipetteButton.setDisabledIcon(getPipetteRolloverIcon());
        pipetteButton.setPressedIcon(getPipetteRolloverIcon());
        pipetteButton.addActionListener(e -> {
            pipetteButton.setEnabled(false);
            pipette.setInitialColor(getColorFromModel());
            pipette.show();
        });
        ((DefaultColorPipette) pipette).setCloseAction(() -> pipetteButton.setEnabled(true));
        return pipetteButton;
    }

    // Java 9 API
    @SuppressWarnings("MissingOverride")
    public boolean isColorTransparencySelectionEnabled() {
        return colorWheelPanel.isColorTransparencySelectionEnabled();
    }

    // Java 9 API
    @SuppressWarnings("MissingOverride")
    public void setColorTransparencySelectionEnabled(final boolean b) {
        boolean oldValue = isColorTransparencySelectionEnabled();
        if (b != oldValue) {
            hexFormatter.setTransparencyEnabled(b);
            colorWheelPanel.setColorTransparencySelectionEnabled(b);
            if (b && getColorFromModel().getAlpha() < 255) {
                colorChanged(ColorUtil.removeAlpha(getColorFromModel()), this);
            }
            firePropertyChange(TRANSPARENCY_ENABLED_PROPERTY, oldValue, b);
        }
    }

    protected Icon getPipetteIcon() {
        return pipetteIcon;
    }

    protected Icon getPipetteRolloverIcon() {
        return pipetteHoverIcon;
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(520, 420);
    }

    @Override
    public void updateChooser() {}

    @Override
    public String getDisplayName() {
        return UIManager.getString("ColorChooser.wheelNameText", getLocale());
    }

    @Override
    public int getMnemonic() {
        return PropertyUtil.getMnemonic("ColorChooser.wheelLabelMnemonic", getLocale());
    }

    @Override
    public int getDisplayedMnemonicIndex() {
        return 6;
    }

    @Override
    public Icon getSmallDisplayIcon() {
        return null;
    }

    @Override
    public Icon getLargeDisplayIcon() {
        return null;
    }
}
