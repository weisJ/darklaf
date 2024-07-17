/*
 * MIT License
 *
 * Copyright (c) 2021-2024 Jannis Weis
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
package com.github.weisj.darklaf.settings;

import java.awt.Color;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.UIManager;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.github.weisj.darklaf.components.ColoredRadioButton;
import com.github.weisj.darklaf.components.DynamicUI;
import com.github.weisj.darklaf.components.color.QuickColorChooser;
import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.Alignment;

public class RadioColorChooser {

    private final ToolTipContext toolTipContext =
            new ToolTipContext().setAlignment(Alignment.CENTER).setCenterAlignment(Alignment.NORTH);
    private final ButtonGroup bg = new ButtonGroup();
    private final List<ColoredRadioButton> radioButtons;
    private final ColoredRadioButton defaultButton;
    private final ColoredRadioButton customButton;

    private final List<ColorSpec> colors;

    private final Color defaultColor;

    public RadioColorChooser(
            @NotNull final List<ColorSpec> colors,
            @NotNull final ColorSpec customColorSpec,
            @Nullable final Color currentColor,
            @NotNull final Color defaultColor) {
        if (colors.isEmpty()) {
            throw new IllegalArgumentException("Must pass at least one color");
        }
        this.colors = colors;
        this.defaultColor = defaultColor;
        ColoredRadioButton[] buttons = new ColoredRadioButton[colors.size() + 1];

        for (int i = 0; i < buttons.length - 1; i++) {
            buttons[i] = createRadioButton(colors.get(i));
        }

        Supplier<Color> colorSupplier = () -> currentColor != null ? currentColor : defaultColor;
        ColoredRadioButton customButton = new ColoredRadioButton(null, null) {
            {
                addActionListener(e -> getColor());
            }

            @Override
            public Color getColor() {
                Color c = super.getColor();
                if (c == null) {
                    Color color = colorSupplier.get();
                    setColors(color, color);
                }
                return super.getColor();
            }
        };
        QuickColorChooser.attachToComponent(customButton, customButton::setColor,
                () -> Optional.ofNullable(customButton.getColor()).orElse(colorSupplier.get()),
                customButton::isSelected);
        buttons[buttons.length - 1] = createRadioButton(customColorSpec, customButton);

        this.radioButtons = Arrays.asList(buttons);
        this.customButton = customButton;
        this.defaultButton = buttons[0];
    }

    public List<ColorSpec> getColors() {
        return colors;
    }

    public List<ColoredRadioButton> getRadioButtons() {
        return radioButtons;
    }

    public void setColor(final Color color) {
        if (color == null) {
            defaultButton.setSelected(true);
            return;
        }
        if (!updateSelection(color)) {
            customButton.setSelected(true);
            if (customButton.getColor() == null) customButton.setColor(color);
        }
    }

    public Color getSelectedColor() {
        Enumeration<AbstractButton> buttons = bg.getElements();
        while (buttons.hasMoreElements()) {
            ColoredRadioButton button = (ColoredRadioButton) buttons.nextElement();
            if (button.isSelected()) {
                if (button == defaultButton) return defaultColor;
                return button.getColor();
            }
        }
        return defaultColor;
    }

    public Color getDefaultColor() {
        return defaultColor;
    }

    public void setEnabled(final boolean enabled) {
        Enumeration<AbstractButton> buttons = bg.getElements();
        while (buttons.hasMoreElements()) {
            buttons.nextElement().setEnabled(enabled);
        }
    }

    private boolean updateSelection(final Color color) {
        Enumeration<AbstractButton> buttons = bg.getElements();
        while (buttons.hasMoreElements()) {
            ColoredRadioButton radioButton = (ColoredRadioButton) buttons.nextElement();
            boolean selected = Objects.equals(radioButton.getColor(), color)
                    || (radioButton == defaultButton && Objects.equals(defaultColor, color));
            bg.setSelected(radioButton.getModel(), selected);
            if (selected) return true;
        }
        return false;
    }

    private ColoredRadioButton createRadioButton(final ColorSpec colorSpec, final ColoredRadioButton button) {
        DynamicUI.withDynamic(button, c -> {
            String name = UIManager.getString(colorSpec.nameKey, c.getLocale());
            c.setName(name);
            c.setToolTipText(name);
        });
        button.putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
        button.putClientProperty(ToolTipConstants.KEY_CONTEXT, toolTipContext);
        bg.add(button);
        return button;
    }

    private ColoredRadioButton createRadioButton(final ColorSpec colorSpec) {
        return createRadioButton(colorSpec, new ColoredRadioButton("", colorSpec.color,
                colorSpec.focusColor != null ? colorSpec.focusColor : colorSpec.color));
    }

    public record ColorSpec(@NotNull Color color, @Nullable Color focusColor, @Nullable String nameKey) {
    }
}
