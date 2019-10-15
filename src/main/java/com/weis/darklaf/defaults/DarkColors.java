/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.weis.darklaf.defaults;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("WeakerAccess")
public class DarkColors {
    protected static DarkColors instance;
    protected Color OptionPaneErrorDialogTitleBackground;
    protected Color OptionPaneErrorDialogTitleForeground;
    protected Color OptionPaneQuestionDialogTitleBackground;
    protected Color OptionPaneQuestionDialogTitleForeground;
    protected Color OptionPaneWarningDialogTitleBackground;
    protected Color OptionPaneWarningDialogTitleForeground;
    protected Color TitleBackground;
    protected Color TitleForeground;
    protected Color TitleInactiveBackground;
    protected Color TitleInactiveForeground;
    protected Color TitleBorderColor;
    protected Color SplitPaneDividerLineColor;
    protected Color ToggleButtonBackground;
    protected Color ToggleButtonInactiveBackground;
    protected Color ToggleButtonFocusBorderColor;
    protected Color ToggleButtonBorderColor;
    protected Color ToggleButtonInactiveBorderColor;
    protected Color ToggleButtonSliderColor;
    protected Color ToggleButtonInactiveSliderColor;
    protected Color ButtonShadowColor;
    protected Color ButtonFocusBorderColor;
    protected Color ButtonDefaultBorderColor;
    protected Color ButtonBorderColor;
    protected Color ButtonInactiveBorderColor;
    protected Color ButtonInactiveForeground;
    protected Color ButtonForeground;
    protected Color ButtonDefaultBackground;
    protected Color ButtonDefaultHoverBackground;
    protected Color ButtonDefaultClickBackground;
    protected Color ButtonBackground;
    protected Color ButtonHoverBackground;
    protected Color ButtonClickBackground;
    protected Color ButtonInactiveBackground;
    protected Color ButtonShadowHoverColor;
    protected Color ButtonShadowClickColor;
    protected Color SliderInactiveTickColor;
    protected Color SliderTrackBackground;
    protected Color SliderSelectedTrackBackground;
    protected Color SliderSelectedTrackInactiveBackground;
    protected Color SliderVolumeSelectedTrackBackground;
    protected Color SliderVolumeSelectedTrackInactiveBackground;
    protected Color SliderThumbBackground;
    protected Color SliderThumbInactiveBackground;
    protected Color SliderVolumeThumbBackground;
    protected Color SliderVolumeThumbInactiveBackground;
    protected Color SliderThumbInactiveBorderColor;
    protected Color SliderThumbBorderColor;
    protected Color SpinnerFocusBorderColor;
    protected Color SpinnerBorderColor;
    protected Color SpinnerInactiveBorderColor;
    protected Color SpinnerBackground;
    protected Color SpinnerInactiveBackground;
    protected Color TextFieldFocusErrorBorderColor;
    protected Color TextFieldFocusBorderColor;
    protected Color TextFieldErrorBorderColor;
    protected Color TextFieldBorderColor;
    protected Color TextFieldInactiveBorderColor;
    protected Color TextFieldBackground;
    protected Color TextFieldInactiveBackground;
    protected Color MenuBarBorderColor;
    protected Color MenuItemAcceleratorForeground;
    protected Color MenuItemAcceleratorSelectionForeground;
    protected Color ComboBoxBackground;
    protected Color ComboBoxInactiveBackground;
    protected Color ComboBoxInactiveForeground;
    protected Color ComboBoxFocusBorderColor;
    protected Color ComboBoxBorderColor;
    protected Color ComboBoxInactiveBorderColor;
    protected Color StatusBarBackground;
    protected Color StatusBarBorderColor;
    protected Color CheckBoxBackground;
    protected Color CheckBoxInactiveBackground;
    protected Color CheckBoxBorderColor;
    protected Color CheckBoxFocusBorderColor;
    protected Color CheckBoxInactiveBorderColor;
    protected Color CheckBoxCheckColor;
    protected Color CheckBoxCheckInactiveColor;
    protected Color TreeAlternativeBackground;
    protected Color TreeLineColor;
    protected Color TreeLineSelectedColor;
    protected Color TreeLineFocusColor;
    protected Color TreeSelectionBackground;
    protected Color TreeFocusSelectionBackground;
    protected Color ScrollBarThumbBorderColor;
    protected Color ScrollBarThumbStartColor;
    protected Color ScrollBarThumbEndColor;
    protected Color ScrollBarTrackBackground;
    protected Color TabbedPaneDragBorderColor;
    protected Color TabbedPaneDropBackground;
    protected Color TabbedPaneTabBorderColor;
    protected Color TabbedPaneAccentColor;
    protected Color TabbedPaneFocusAccentColor;
    protected Color TabbedPaneTabSelectedHoverBackground;
    protected Color TabbedPaneTabSelectedBackground;
    protected Color TabbedPaneTabHoverBackground;
    protected Color TableHeaderBorderColor;
    protected Color ErrorGlow;
    protected Color ErrorFocusGlow;
    protected Color FocusGlow;
    protected Color WarningGlow;
    protected Color TitleCloseClickBackground;
    protected Color TitleCloseHoverBackground;
    protected Color TreeEditorBorderColor;
    protected Color TabbedPaneTabAreaBackground;
    protected Color ToolBarDropBackground;
    protected Color ToolBarBackground;
    protected Color ListFocusBorderColor;
    protected Color TabFrameTabForeground;
    protected Color TabFrameTabSelectedBackground;
    protected Color TabFrameTabHoverBackground;
    protected Color TabFrameTabSelectedForeground;
    protected Color TabFramePopupHeaderHoverBackground;
    protected Color TabFramePopupHeaderSelectedBackground;
    protected Color TabFramePopupHeaderSelectedHoverBackground;
    protected Color TabFramePopupHeaderFocusHoverBackground;
    protected Color TabFramePopupHeaderFocusSelectedBackground;
    protected Color TabFramePopupHeaderFocusSelectedHoverBackground;
    protected Color TabFramePopupHeaderBackground;
    protected Color TabFramePopupButtonHoverBackground;
    protected Color TabFramePopupButtonClickBackground;
    protected Color TabFramePopupFocusHeaderBackground;
    protected Color TabFramePopupButtonFocusHoverBackground;
    protected Color TabFramePopupButtonFocusClickBackground;
    protected Color TabFramePopupBorderColor;
    protected Color TabFrameBorderColor;
    protected Color FileChooserListViewBorderColor;
    protected Color FilePaneListViewBackground;
    protected Color TableRowBorderColor;
    protected Color TableFocusBorderColor;
    protected Color TableHeaderBackground;
    protected Color TableAlternativeBackground;
    protected Color TableDropLineColor;
    protected Color TableDropLineShortColor;
    protected Color TableBackground;
    protected Color ColorChooserPreviewBorderColor;
    protected Color RadioButtonBackground;
    protected Color RadioButtonInactiveBackground;
    protected Color RadioButtonFocusBorderColor;
    protected Color RadioButtonBorderColor;
    protected Color RadioButtonInactiveBorderColor;
    protected Color RadioButtonCheckColor;
    protected Color RadioButtonCheckInactiveColor;
    protected Color DesktopIconHoverBackground;
    protected Color DesktopIconClickBackground;
    protected Color InternalFrameBorderShadowColor;
    protected Color InternalFrameTitleBackground;
    protected Color InternalFrameTitleSelectedBackground;
    protected Color InternalFrameTitleForeground;
    protected Color InternalFrameSelectedButtonBackground;
    protected Color InternalFrameSelectedButtonHoverBackground;
    protected Color InternalFrameSelectedButtonClickBackground;
    protected Color InternalFrameButtonBackground;
    protected Color InternalFrameButtonHoverBackground;
    protected Color InternalFrameButtonClickBackground;
    protected Color InternalFrameBorderColor;
    protected Color InternalFrameTitleSelectedForeground;
    protected Color MenuItemBackground;
    protected Color MenuItemForeground;
    protected Color MenuItemSelectionForeground;
    protected Color TabbedPaneSelectedForeground;
    protected Color TaskPaneBorderColor;
    protected Color TaskPaneTitleBorderColor;
    protected Color ToolBarDockingBackground;
    protected Color ToolBarFloatingBackground;
    protected Color ToolBarDockingBorderColor;
    protected Color ToolBarFloatingBorderColor;
    protected Color TableFocusSelectionBackground;
    protected Color TableSelectionBackground;
    protected Color TableGridColor;
    protected Color ColorChooserDefaultRecentColor;
    protected Color ColorChooserSliderBorderColor;
    protected Color ColorChooserSliderShadowColor;
    protected Color ColorChooserSliderKnobColor;
    protected Color ColorChooserColorWheelDropBackground;
    protected Color ColorChooserColorWheelDropBorderColor;
    protected Color ColorChooserSwatchBorderColor;
    protected Color ColorChooserPipetteBorderColor;
    protected Color ColorChooserSwatchGridColor;
    protected Color PanelBackground;
    protected Color DesktopIconBorderColor;
    protected Color ToolTipBorderColor;
    protected Color ToolTipShadowColor;
    protected Color ProgressBarTrackBackground;
    protected Color ProgressBarProgressColor;
    protected Color ProgressBarIndeterminateStartColor;
    protected Color ProgressBarIndeterminateEndColor;
    protected Color ProgressBarFailedColor;
    protected Color ProgressBarFailedEndColor;
    protected Color ProgressBarPassedColor;
    protected Color ProgressBarPassedEndColor;

    public static DarkColors get() {
        if (instance == null) {
            instance = new DarkColors();
        }
        return instance;
    }

    public static void uninstall() {
        if (instance == null) return;
        instance.uninstallColor();
        instance = null;
    }

    protected void uninstallColor() {
        OptionPaneErrorDialogTitleBackground = null;
        OptionPaneErrorDialogTitleForeground = null;
        OptionPaneQuestionDialogTitleBackground = null;
        OptionPaneQuestionDialogTitleForeground = null;
        OptionPaneWarningDialogTitleBackground = null;
        OptionPaneWarningDialogTitleForeground = null;
        TitleBackground = null;
        TitleForeground = null;
        TitleInactiveBackground = null;
        TitleInactiveForeground = null;
        TitleBorderColor = null;
        SplitPaneDividerLineColor = null;
        ToggleButtonBackground = null;
        ToggleButtonInactiveBackground = null;
        ToggleButtonFocusBorderColor = null;
        ToggleButtonBorderColor = null;
        ToggleButtonInactiveBorderColor = null;
        ToggleButtonSliderColor = null;
        ToggleButtonInactiveSliderColor = null;
        ButtonShadowColor = null;
        ButtonFocusBorderColor = null;
        ButtonDefaultBorderColor = null;
        ButtonBorderColor = null;
        ButtonInactiveBorderColor = null;
        ButtonInactiveForeground = null;
        ButtonForeground = null;
        ButtonDefaultBackground = null;
        ButtonDefaultHoverBackground = null;
        ButtonDefaultClickBackground = null;
        ButtonBackground = null;
        ButtonHoverBackground = null;
        ButtonClickBackground = null;
        ButtonInactiveBackground = null;
        ButtonShadowHoverColor = null;
        ButtonShadowClickColor = null;
        SliderInactiveTickColor = null;
        SliderTrackBackground = null;
        SliderSelectedTrackBackground = null;
        SliderSelectedTrackInactiveBackground = null;
        SliderVolumeSelectedTrackBackground = null;
        SliderVolumeSelectedTrackInactiveBackground = null;
        SliderThumbBackground = null;
        SliderThumbInactiveBackground = null;
        SliderVolumeThumbBackground = null;
        SliderVolumeThumbInactiveBackground = null;
        SliderThumbInactiveBorderColor = null;
        SliderThumbBorderColor = null;
        SpinnerFocusBorderColor = null;
        SpinnerBorderColor = null;
        SpinnerInactiveBorderColor = null;
        SpinnerBackground = null;
        SpinnerInactiveBackground = null;
        TextFieldFocusErrorBorderColor = null;
        TextFieldFocusBorderColor = null;
        TextFieldErrorBorderColor = null;
        TextFieldBorderColor = null;
        TextFieldInactiveBorderColor = null;
        TextFieldBackground = null;
        TextFieldInactiveBackground = null;
        MenuBarBorderColor = null;
        MenuItemAcceleratorForeground = null;
        MenuItemAcceleratorSelectionForeground = null;
        ComboBoxBackground = null;
        ComboBoxInactiveBackground = null;
        ComboBoxInactiveForeground = null;
        ComboBoxFocusBorderColor = null;
        ComboBoxBorderColor = null;
        ComboBoxInactiveBorderColor = null;
        StatusBarBackground = null;
        StatusBarBorderColor = null;
        CheckBoxBackground = null;
        CheckBoxInactiveBackground = null;
        CheckBoxBorderColor = null;
        CheckBoxFocusBorderColor = null;
        CheckBoxInactiveBorderColor = null;
        CheckBoxCheckColor = null;
        CheckBoxCheckInactiveColor = null;
        TreeAlternativeBackground = null;
        TreeLineColor = null;
        TreeLineSelectedColor = null;
        TreeLineFocusColor = null;
        TreeSelectionBackground = null;
        TreeFocusSelectionBackground = null;
        ScrollBarThumbBorderColor = null;
        ScrollBarThumbStartColor = null;
        ScrollBarThumbEndColor = null;
        ScrollBarTrackBackground = null;
        TabbedPaneDragBorderColor = null;
        TabbedPaneDropBackground = null;
        TabbedPaneTabBorderColor = null;
        TabbedPaneAccentColor = null;
        TabbedPaneFocusAccentColor = null;
        TabbedPaneTabSelectedHoverBackground = null;
        TabbedPaneTabSelectedBackground = null;
        TabbedPaneTabHoverBackground = null;
        TableHeaderBorderColor = null;
        ErrorGlow = null;
        ErrorFocusGlow = null;
        FocusGlow = null;
        WarningGlow = null;
        TitleCloseClickBackground = null;
        TitleCloseHoverBackground = null;
        TreeEditorBorderColor = null;
        TabbedPaneTabAreaBackground = null;
        ToolBarDropBackground = null;
        ToolBarBackground = null;
        ListFocusBorderColor = null;
        TabFrameTabForeground = null;
        TabFrameTabSelectedBackground = null;
        TabFrameTabHoverBackground = null;
        TabFrameTabSelectedForeground = null;
        TabFramePopupHeaderHoverBackground = null;
        TabFramePopupHeaderSelectedBackground = null;
        TabFramePopupHeaderSelectedHoverBackground = null;
        TabFramePopupHeaderFocusHoverBackground = null;
        TabFramePopupHeaderFocusSelectedBackground = null;
        TabFramePopupHeaderFocusSelectedHoverBackground = null;
        TabFramePopupHeaderBackground = null;
        TabFramePopupButtonHoverBackground = null;
        TabFramePopupButtonClickBackground = null;
        TabFramePopupFocusHeaderBackground = null;
        TabFramePopupButtonFocusHoverBackground = null;
        TabFramePopupButtonFocusClickBackground = null;
        TabFramePopupBorderColor = null;
        TabFrameBorderColor = null;
        FileChooserListViewBorderColor = null;
        FilePaneListViewBackground = null;
        TableRowBorderColor = null;
        TableFocusBorderColor = null;
        TableHeaderBackground = null;
        TableAlternativeBackground = null;
        TableDropLineColor = null;
        TableDropLineShortColor = null;
        TableBackground = null;
        ColorChooserPreviewBorderColor = null;
        RadioButtonBackground = null;
        RadioButtonInactiveBackground = null;
        RadioButtonFocusBorderColor = null;
        RadioButtonBorderColor = null;
        RadioButtonInactiveBorderColor = null;
        RadioButtonCheckColor = null;
        RadioButtonCheckInactiveColor = null;
        DesktopIconHoverBackground = null;
        DesktopIconClickBackground = null;
        InternalFrameBorderShadowColor = null;
        InternalFrameTitleBackground = null;
        InternalFrameTitleSelectedBackground = null;
        InternalFrameTitleForeground = null;
        InternalFrameSelectedButtonBackground = null;
        InternalFrameSelectedButtonHoverBackground = null;
        InternalFrameSelectedButtonClickBackground = null;
        InternalFrameButtonBackground = null;
        InternalFrameButtonHoverBackground = null;
        InternalFrameButtonClickBackground = null;
        InternalFrameBorderColor = null;
        InternalFrameTitleSelectedForeground = null;
        MenuItemBackground = null;
        MenuItemForeground = null;
        MenuItemSelectionForeground = null;
        TabbedPaneSelectedForeground = null;
        TaskPaneBorderColor = null;
        TaskPaneTitleBorderColor = null;
        ToolBarDockingBackground = null;
        ToolBarFloatingBackground = null;
        ToolBarDockingBorderColor = null;
        ToolBarFloatingBorderColor = null;
        TableFocusSelectionBackground = null;
        TableSelectionBackground = null;
        TableGridColor = null;
        ColorChooserDefaultRecentColor = null;
        ColorChooserSliderBorderColor = null;
        ColorChooserSliderShadowColor = null;
        ColorChooserSliderKnobColor = null;
        ColorChooserColorWheelDropBackground = null;
        ColorChooserColorWheelDropBorderColor = null;
        ColorChooserSwatchBorderColor = null;
        ColorChooserPipetteBorderColor = null;
        ColorChooserSwatchGridColor = null;
        PanelBackground = null;
        DesktopIconBorderColor = null;
        ToolTipBorderColor = null;
        ToolTipShadowColor = null;
        ProgressBarTrackBackground = null;
        ProgressBarProgressColor = null;
        ProgressBarIndeterminateStartColor = null;
        ProgressBarIndeterminateEndColor = null;
        ProgressBarFailedColor = null;
        ProgressBarFailedEndColor = null;
        ProgressBarPassedColor = null;
        ProgressBarPassedEndColor = null;
    }

    //TitlePane

    public Color getOptionPaneErrorDialogTitleBackground() {
        if (OptionPaneErrorDialogTitleBackground == null) {
            OptionPaneErrorDialogTitleBackground = UIManager.getColor("OptionPane.errorDialog.titlePane.background");
        }
        return OptionPaneErrorDialogTitleBackground;
    }

    public Color getOptionPaneErrorDialogTitleForeground() {
        if (OptionPaneErrorDialogTitleForeground == null) {
            OptionPaneErrorDialogTitleForeground = UIManager.getColor("OptionPane.errorDialog.titlePane.foreground");
        }
        return OptionPaneErrorDialogTitleForeground;
    }

    public Color getOptionPaneQuestionDialogTitleBackground() {
        if (OptionPaneQuestionDialogTitleBackground == null) {
            OptionPaneQuestionDialogTitleBackground = UIManager.getColor("OptionPane.questionDialog.titlePane.background");
        }
        return OptionPaneQuestionDialogTitleBackground;
    }

    public Color getOptionPaneQuestionDialogTitleForeground() {
        if (OptionPaneQuestionDialogTitleForeground == null) {
            OptionPaneQuestionDialogTitleForeground = UIManager.getColor("OptionPane.questionDialog.titlePane.foreground");
        }
        return OptionPaneQuestionDialogTitleForeground;
    }

    public Color getOptionPaneWarningDialogTitleBackground() {
        if (OptionPaneWarningDialogTitleBackground == null) {
            OptionPaneWarningDialogTitleBackground = UIManager.getColor("OptionPane.warningDialog.titlePane.background");
        }
        return OptionPaneWarningDialogTitleBackground;
    }

    public Color getOptionPaneWarningDialogTitleForeground() {
        if (OptionPaneWarningDialogTitleForeground == null) {
            OptionPaneWarningDialogTitleForeground = UIManager.getColor("OptionPane.warningDialog.titlePane.foreground");
        }
        return OptionPaneWarningDialogTitleForeground;
    }

    public Color getTitleBackground() {
        if (TitleBackground == null) {
            TitleBackground = UIManager.getColor("TitlePane.background");
        }
        return TitleBackground;
    }

    public Color getTitleForeground() {
        if (TitleForeground == null) {
            TitleForeground = UIManager.getColor("TitlePane.foreground");
        }
        return TitleForeground;
    }

    public Color getTitleInactiveBackground() {
        if (TitleInactiveBackground == null) {
            TitleInactiveBackground = UIManager.getColor("TitlePane.inactiveBackground");
        }
        return TitleInactiveBackground;
    }

    public Color getTitleInactiveForeground() {
        if (TitleInactiveForeground == null) {
            TitleInactiveForeground = UIManager.getColor("TitlePane.inactiveForeground");
        }
        return TitleInactiveForeground;
    }

    public Color getTitleBorderColor() {
        if (TitleBorderColor == null) {
            TitleBorderColor = UIManager.getColor("TitlePane.borderColor");
        }
        return TitleBorderColor;
    }

    public Color getTitleCloseClickBackground() {
        if (TitleCloseClickBackground == null) {
            TitleCloseClickBackground = UIManager.getColor("TitlePane.close.clickColor");
        }
        return TitleCloseClickBackground;
    }

    public Color getTitleCloseHoverBackground() {
        if (TitleCloseHoverBackground == null) {
            TitleCloseHoverBackground = UIManager.getColor("TitlePane.close.rollOverColor");
        }
        return TitleCloseHoverBackground;
    }

    //ToggleButton

    public Color getSplitPaneDividerLineColor() {
        if (SplitPaneDividerLineColor == null) {
            SplitPaneDividerLineColor = UIManager.getColor("SplitPane.dividerLineColor");
        }
        return SplitPaneDividerLineColor;
    }

    public Color getToggleButtonBackground() {
        if (ToggleButtonBackground == null) {
            ToggleButtonBackground = UIManager.getColor("ToggleButton.activeFillColor");
        }
        return ToggleButtonBackground;
    }

    public Color getToggleButtonInactiveBackground() {
        if (ToggleButtonInactiveBackground == null) {
            ToggleButtonInactiveBackground = UIManager.getColor("ToggleButton.inactiveFillColor");
        }
        return ToggleButtonInactiveBackground;
    }

    public Color getToggleButtonFocusBorderColor() {
        if (ToggleButtonFocusBorderColor == null) {
            ToggleButtonFocusBorderColor = UIManager.getColor("ToggleButton.focusedSliderBorderColor");
        }
        return ToggleButtonFocusBorderColor;
    }

    public Color getToggleButtonBorderColor() {
        if (ToggleButtonBorderColor == null) {
            ToggleButtonBorderColor = UIManager.getColor("ToggleButton.sliderBorderColor");
        }
        return ToggleButtonBorderColor;
    }

    public Color getToggleButtonInactiveBorderColor() {
        if (ToggleButtonInactiveBorderColor == null) {
            ToggleButtonInactiveBorderColor = UIManager.getColor("ToggleButton.disabledSliderBorderColor");
        }
        return ToggleButtonInactiveBorderColor;
    }

    public Color getToggleButtonSliderColor() {
        if (ToggleButtonSliderColor == null) {
            ToggleButtonSliderColor = UIManager.getColor("ToggleButton.sliderColor");
        }
        return ToggleButtonSliderColor;
    }

    public Color getToggleButtonInactiveSliderColor() {
        if (ToggleButtonInactiveSliderColor == null) {
            ToggleButtonInactiveSliderColor = UIManager.getColor("ToggleButton.disabledSliderColor");
        }
        return ToggleButtonInactiveSliderColor;
    }

    //Button

    public Color getButtonShadowColor() {
        if (ButtonShadowColor == null) {
            ButtonShadowColor = UIManager.getColor("Button.shadow");
        }
        return ButtonShadowColor;
    }

    public Color getButtonFocusBorderColor() {
        if (ButtonFocusBorderColor == null) {
            ButtonFocusBorderColor = UIManager.getColor("Button.focusBorderColor");
        }
        return ButtonFocusBorderColor;
    }

    public Color getButtonDefaultBorderColor() {
        if (ButtonDefaultBorderColor == null) {
            ButtonDefaultBorderColor = UIManager.getColor("Button.defaultBorderColor");
        }
        return ButtonDefaultBorderColor;
    }

    public Color getButtonBorderColor() {
        if (ButtonBorderColor == null) {
            ButtonBorderColor = UIManager.getColor("Button.activeBorderColor");
        }
        return ButtonBorderColor;
    }

    public Color getButtonInactiveBorderColor() {
        if (ButtonInactiveBorderColor == null) {
            ButtonInactiveBorderColor = UIManager.getColor("Button.inactiveBorderColor");
        }
        return ButtonInactiveBorderColor;
    }

    public Color getButtonInactiveForeground() {
        if (ButtonInactiveForeground == null) {
            ButtonInactiveForeground = UIManager.getColor("Button.disabledText");
        }
        return ButtonInactiveForeground;
    }

    public Color getButtonForeground() {
        if (ButtonForeground == null) {
            ButtonForeground = UIManager.getColor("Button.selectedButtonForeground");
        }
        return ButtonForeground;
    }

    public Color getButtonDefaultBackground() {
        if (ButtonDefaultBackground == null) {
            ButtonDefaultBackground = UIManager.getColor("Button.defaultFillColor");
        }
        return ButtonDefaultBackground;
    }

    public Color getButtonDefaultHoverBackground() {
        if (ButtonDefaultHoverBackground == null) {
            ButtonDefaultHoverBackground = UIManager.getColor("Button.defaultFillColorRollOver");
        }
        return ButtonDefaultHoverBackground;
    }

    public Color getButtonDefaultClickBackground() {
        if (ButtonDefaultClickBackground == null) {
            ButtonDefaultClickBackground = UIManager.getColor("Button.defaultFillColorClick");
        }
        return ButtonDefaultClickBackground;
    }

    public Color getButtonBackground() {
        if (ButtonBackground == null) {
            ButtonBackground = UIManager.getColor("Button.activeFillColor");
        }
        return ButtonBackground;
    }

    public Color getButtonHoverBackground() {
        if (ButtonHoverBackground == null) {
            ButtonHoverBackground = UIManager.getColor("Button.activeFillColorRollOver");
        }
        return ButtonHoverBackground;
    }

    public Color getButtonClickBackground() {
        if (ButtonClickBackground == null) {
            ButtonClickBackground = UIManager.getColor("Button.activeFillColorClick");
        }
        return ButtonClickBackground;
    }

    public Color getButtonInactiveBackground() {
        if (ButtonInactiveBackground == null) {
            ButtonInactiveBackground = UIManager.getColor("Button.inactiveFillColor");
        }
        return ButtonInactiveBackground;
    }

    public Color getButtonShadowHoverColor() {
        if (ButtonShadowHoverColor == null) {
            ButtonShadowHoverColor = UIManager.getColor("Button.shadow.hover");
        }
        return ButtonShadowHoverColor;
    }

    public Color getButtonShadowClickColor() {
        if (ButtonShadowClickColor == null) {
            ButtonShadowClickColor = UIManager.getColor("Button.shadow.click");
        }
        return ButtonShadowClickColor;
    }

    //Slider

    public Color getSliderInactiveTickColor() {
        if (SliderInactiveTickColor == null) {
            SliderInactiveTickColor = UIManager.getColor("Slider.disabledTickColor");
        }
        return SliderInactiveTickColor;
    }

    public Color getSliderTrackBackground() {
        if (SliderTrackBackground == null) {
            SliderTrackBackground = UIManager.getColor("Slider.trackBackground");
        }
        return SliderTrackBackground;
    }

    public Color getSliderSelectedTrackBackground() {
        if (SliderSelectedTrackBackground == null) {
            SliderSelectedTrackBackground = UIManager.getColor("Slider.selectedTrackColor");
        }
        return SliderSelectedTrackBackground;
    }

    public Color getSliderSelectedTrackInactiveBackground() {
        if (SliderSelectedTrackInactiveBackground == null) {
            SliderSelectedTrackInactiveBackground = UIManager.getColor("Slider.disabledTrackColor");
        }
        return SliderSelectedTrackInactiveBackground;
    }

    public Color getSliderVolumeSelectedTrackBackground() {
        if (SliderVolumeSelectedTrackBackground == null) {
            SliderVolumeSelectedTrackBackground = UIManager.getColor("Slider.volume.selectedTrackColor");
        }
        return SliderVolumeSelectedTrackBackground;
    }

    public Color getSliderVolumeSelectedTrackInactiveBackground() {
        if (SliderVolumeSelectedTrackInactiveBackground == null) {
            SliderVolumeSelectedTrackInactiveBackground = UIManager.getColor("Slider.volume.disabledTrackColor");
        }
        return SliderVolumeSelectedTrackInactiveBackground;
    }

    public Color getSliderThumbBackground() {
        if (SliderThumbBackground == null) {
            SliderThumbBackground = UIManager.getColor("Slider.activeThumbFill");
        }
        return SliderThumbBackground;
    }

    public Color getSliderThumbInactiveBackground() {
        if (SliderThumbInactiveBackground == null) {
            SliderThumbInactiveBackground = UIManager.getColor("Slider.inactiveThumbFill");
        }
        return SliderThumbInactiveBackground;
    }

    public Color getSliderVolumeThumbBackground() {
        if (SliderVolumeThumbBackground == null) {
            SliderVolumeThumbBackground = UIManager.getColor("Slider.volume.activeThumbFill");
        }
        return SliderVolumeThumbBackground;
    }

    public Color getSliderVolumeThumbInactiveBackground() {
        if (SliderVolumeThumbInactiveBackground == null) {
            SliderVolumeThumbInactiveBackground = UIManager.getColor("Slider.volume.inactiveThumbFill");
        }
        return SliderVolumeThumbInactiveBackground;
    }

    public Color getSliderThumbInactiveBorderColor() {
        if (SliderThumbInactiveBorderColor == null) {
            SliderThumbInactiveBorderColor = UIManager.getColor("Slider.thumbBorderColor");
        }
        return SliderThumbInactiveBorderColor;
    }

    public Color getSliderThumbBorderColor() {
        if (SliderThumbBorderColor == null) {
            SliderThumbBorderColor = UIManager.getColor("Slider.thumbBorderColorDisabled");
        }
        return SliderThumbBorderColor;
    }

    //Spinner

    public Color getSpinnerFocusBorderColor() {
        if (SpinnerFocusBorderColor == null) {
            SpinnerFocusBorderColor = UIManager.getColor("Spinner.focusBorderColor");
        }
        return SpinnerFocusBorderColor;
    }

    public Color getSpinnerBorderColor() {
        if (SpinnerBorderColor == null) {
            SpinnerBorderColor = UIManager.getColor("Spinner.activeBorderColor");
        }
        return SpinnerBorderColor;
    }

    public Color getSpinnerInactiveBorderColor() {
        if (SpinnerInactiveBorderColor == null) {
            SpinnerInactiveBorderColor = UIManager.getColor("Spinner.inactiveBorderColor");
        }
        return SpinnerInactiveBorderColor;
    }

    public Color getSpinnerBackground() {
        if (SpinnerBackground == null) {
            SpinnerBackground = UIManager.getColor("Spinner.activeBackground");
        }
        return SpinnerBackground;
    }

    public Color getSpinnerInactiveBackground() {
        if (SpinnerInactiveBackground == null) {
            SpinnerInactiveBackground = UIManager.getColor("Spinner.inactiveBackground");
        }
        return SpinnerInactiveBackground;
    }

    //TextField

    public Color getTextFieldFocusErrorBorderColor() {
        if (TextFieldFocusErrorBorderColor == null) {
            TextFieldFocusErrorBorderColor = UIManager.getColor("TextField.border.focusError");
        }
        return TextFieldFocusErrorBorderColor;
    }

    public Color getTextFieldFocusBorderColor() {
        if (TextFieldFocusBorderColor == null) {
            TextFieldFocusBorderColor = UIManager.getColor("TextField.border.focus");
        }
        return TextFieldFocusBorderColor;
    }

    public Color getTextFieldErrorBorderColor() {
        if (TextFieldErrorBorderColor == null) {
            TextFieldErrorBorderColor = UIManager.getColor("TextField.border.error");
        }
        return TextFieldErrorBorderColor;
    }

    public Color getTextFieldBorderColor() {
        if (TextFieldBorderColor == null) {
            TextFieldBorderColor = UIManager.getColor("TextField.border.enabled");
        }
        return TextFieldBorderColor;
    }

    public Color getTextFieldInactiveBorderColor() {
        if (TextFieldInactiveBorderColor == null) {
            TextFieldInactiveBorderColor = UIManager.getColor("TextField.border.disabled");
        }
        return TextFieldInactiveBorderColor;
    }

    public Color getTextFieldBackground() {
        if (TextFieldBackground == null) {
            TextFieldBackground = UIManager.getColor("TextField.background");
        }
        return TextFieldBackground;
    }

    public Color getTextFieldInactiveBackground() {
        if (TextFieldInactiveBackground == null) {
            TextFieldInactiveBackground = UIManager.getColor("TextField.disabledBackground");
        }
        return TextFieldInactiveBackground;
    }

    //Menu

    public Color getMenuBarBorderColor() {
        if (MenuBarBorderColor == null) {
            MenuBarBorderColor = UIManager.getColor("MenuBar.borderColor");
        }
        return MenuBarBorderColor;
    }

    public Color getMenuItemAcceleratorForeground() {
        if (MenuItemAcceleratorForeground == null) {
            MenuItemAcceleratorForeground = UIManager.getColor("MenuItem.foreground");
        }
        return MenuItemAcceleratorForeground;
    }

    public Color getMenuItemAcceleratorSelectionForeground() {
        if (MenuItemAcceleratorSelectionForeground == null) {
            MenuItemAcceleratorSelectionForeground = UIManager.getColor("MenuItem.selectionForeground");
        }
        return MenuItemAcceleratorSelectionForeground;
    }

    public Color getMenuItemBackground() {
        if (MenuItemBackground == null) {
            MenuItemBackground = UIManager.getColor("MenuItem.background");
        }
        return MenuItemBackground;
    }

    public Color getMenuItemForeground() {
        if (MenuItemForeground == null) {
            MenuItemForeground = UIManager.getColor("MenuItem.foreground");
        }
        return MenuItemForeground;
    }

    public Color getMenuItemSelectionForeground() {
        if (MenuItemSelectionForeground == null) {
            MenuItemSelectionForeground = UIManager.getColor("MenuItem.selectionForeground");
        }
        return MenuItemSelectionForeground;
    }

    //ComboBox

    public Color getComboBoxBackground() {
        if (ComboBoxBackground == null) {
            ComboBoxBackground = UIManager.getColor("ComboBox.activeBackground");
        }
        return ComboBoxBackground;
    }

    public Color getComboBoxInactiveBackground() {
        if (ComboBoxInactiveBackground == null) {
            ComboBoxInactiveBackground = UIManager.getColor("ComboBox.inactiveBackground");
        }
        return ComboBoxInactiveBackground;
    }

    public Color getComboBoxInactiveForeground() {
        if (ComboBoxInactiveForeground == null) {
            ComboBoxInactiveForeground = UIManager.getColor("ComboBox.disabledForeground");
        }
        return ComboBoxInactiveForeground;
    }

    public Color getComboBoxFocusBorderColor() {
        if (ComboBoxFocusBorderColor == null) {
            ComboBoxFocusBorderColor = UIManager.getColor("ComboBox.focusBorderColor");
        }
        return ComboBoxFocusBorderColor;
    }

    public Color getComboBoxBorderColor() {
        if (ComboBoxBorderColor == null) {
            ComboBoxBorderColor = UIManager.getColor("ComboBox.activeBorderColor");
        }
        return ComboBoxBorderColor;
    }

    public Color getComboBoxInactiveBorderColor() {
        if (ComboBoxInactiveBorderColor == null) {
            ComboBoxInactiveBorderColor = UIManager.getColor("ComboBox.inactiveBorderColor");
        }
        return ComboBoxInactiveBorderColor;
    }

    //StatusBar

    public Color getStatusBarBackground() {
        if (StatusBarBackground == null) {
            StatusBarBackground = UIManager.getColor("StatusBar.background");
        }
        return StatusBarBackground;
    }

    public Color getStatusBarBorderColor() {
        if (StatusBarBorderColor == null) {
            StatusBarBorderColor = UIManager.getColor("StatusBar.topColor");
        }
        return StatusBarBorderColor;
    }

    //CheckBox

    public Color getCheckBoxBackground() {
        if (CheckBoxBackground == null) {
            CheckBoxBackground = UIManager.getColor("CheckBox.activeFillColor");
        }
        return CheckBoxBackground;
    }

    public Color getCheckBoxInactiveBackground() {
        if (CheckBoxInactiveBackground == null) {
            CheckBoxInactiveBackground = UIManager.getColor("CheckBox.inactiveFillColor");
        }
        return CheckBoxInactiveBackground;
    }

    public Color getCheckBoxBorderColor() {
        if (CheckBoxBorderColor == null) {
            CheckBoxBorderColor = UIManager.getColor("CheckBox.activeBorderColor");
        }
        return CheckBoxBorderColor;
    }

    public Color getCheckBoxFocusBorderColor() {
        if (CheckBoxFocusBorderColor == null) {
            CheckBoxFocusBorderColor = UIManager.getColor("CheckBox.focusBorderColor");
        }
        return CheckBoxFocusBorderColor;
    }

    public Color getCheckBoxInactiveBorderColor() {
        if (CheckBoxInactiveBorderColor == null) {
            CheckBoxInactiveBorderColor = UIManager.getColor("CheckBox.inactiveBorderColor");
        }
        return CheckBoxInactiveBorderColor;
    }

    public Color getCheckBoxCheckColor() {
        if (CheckBoxCheckColor == null) {
            CheckBoxCheckColor = UIManager.getColor("CheckBox.selectionEnabledColor");
        }
        return CheckBoxCheckColor;
    }

    public Color getCheckBoxCheckInactiveColor() {
        if (CheckBoxCheckInactiveColor == null) {
            CheckBoxCheckInactiveColor = UIManager.getColor("CheckBox.selectionDisabledColor");
        }
        return CheckBoxCheckInactiveColor;
    }

    //Tree

    public Color getTreeAlternativeBackground() {
        if (TreeAlternativeBackground == null) {
            TreeAlternativeBackground = UIManager.getColor("Tree.alternateRowBackground");
        }
        return TreeAlternativeBackground;
    }

    public Color getTreeLineColor() {
        if (TreeLineColor == null) {
            TreeLineColor = UIManager.getColor("Tree.lineUnselected");
        }
        return TreeLineColor;
    }

    public Color getTreeLineSelectedColor() {
        if (TreeLineSelectedColor == null) {
            TreeLineSelectedColor = UIManager.getColor("Tree.lineSelected");
        }
        return TreeLineSelectedColor;
    }

    public Color getTreeLineFocusColor() {
        if (TreeLineFocusColor == null) {
            TreeLineFocusColor = UIManager.getColor("Tree.lineFocusSelected");
        }
        return TreeLineFocusColor;
    }

    public Color getTreeSelectionBackground() {
        if (TreeSelectionBackground == null) {
            TreeSelectionBackground = UIManager.getColor("Tree.unfocusedSelectionBackground");
        }
        return TreeSelectionBackground;
    }

    public Color getTreeFocusSelectionBackground() {
        if (TreeFocusSelectionBackground == null) {
            TreeFocusSelectionBackground = UIManager.getColor("Tree.selectionBackground");
        }
        return TreeFocusSelectionBackground;
    }

    public Color getTreeEditorBorderColor() {
        if (TreeEditorBorderColor == null) {
            TreeEditorBorderColor = UIManager.getColor("Tree.editorBorderColor");
        }
        return TreeEditorBorderColor;
    }

    //ScrollBar

    public Color getScrollBarThumbBorderColor() {
        if (ScrollBarThumbBorderColor == null) {
            ScrollBarThumbBorderColor = UIManager.getColor("ScrollBar.thumbBorderColor");
        }
        return ScrollBarThumbBorderColor;
    }

    public Color getScrollBarThumbStartColor() {
        if (ScrollBarThumbStartColor == null) {
            ScrollBarThumbStartColor = UIManager.getColor("ScrollBar.fadeStartColor");
        }
        return ScrollBarThumbStartColor;
    }

    public Color getScrollBarThumbEndColor() {
        if (ScrollBarThumbEndColor == null) {
            ScrollBarThumbEndColor = UIManager.getColor("ScrollBar.fadeEndColor");
        }
        return ScrollBarThumbEndColor;
    }

    public Color getScrollBarTrackBackground() {
        if (ScrollBarTrackBackground == null) {
            ScrollBarTrackBackground = UIManager.getColor("ScrollBar.trackColor");
        }
        return ScrollBarTrackBackground;
    }

    //TabbedPane

    public Color getTabbedPaneDragBorderColor() {
        if (TabbedPaneDragBorderColor == null) {
            TabbedPaneDragBorderColor = UIManager.getColor("TabbedPane.dragBorderColor");
        }
        return TabbedPaneDragBorderColor;
    }

    public Color getTabbedPaneDropBackground() {
        if (TabbedPaneDropBackground == null) {
            TabbedPaneDropBackground = UIManager.getColor("TabbedPane.dropFill");
        }
        return TabbedPaneDropBackground;
    }

    public Color getTabbedPaneTabBorderColor() {
        if (TabbedPaneTabBorderColor == null) {
            TabbedPaneTabBorderColor = UIManager.getColor("TabbedPane.tabBorderColor");
        }
        return TabbedPaneTabBorderColor;
    }

    public Color getTabbedPaneAccentColor() {
        if (TabbedPaneAccentColor == null) {
            TabbedPaneAccentColor = UIManager.getColor("TabbedPane.accent");
        }
        return TabbedPaneAccentColor;
    }

    public Color getTabbedPaneFocusAccentColor() {
        if (TabbedPaneFocusAccentColor == null) {
            TabbedPaneFocusAccentColor = UIManager.getColor("TabbedPane.accentFocus");
        }
        return TabbedPaneFocusAccentColor;
    }

    public Color getTabbedPaneTabSelectedHoverBackground() {
        if (TabbedPaneTabSelectedHoverBackground == null) {
            TabbedPaneTabSelectedHoverBackground = UIManager.getColor("TabbedPane.selectedHoverBackground");
        }
        return TabbedPaneTabSelectedHoverBackground;
    }

    public Color getTabbedPaneTabSelectedBackground() {
        if (TabbedPaneTabSelectedBackground == null) {
            TabbedPaneTabSelectedBackground = UIManager.getColor("TabbedPane.selectedBackground");
        }
        return TabbedPaneTabSelectedBackground;
    }

    public Color getTabbedPaneTabHoverBackground() {
        if (TabbedPaneTabHoverBackground == null) {
            TabbedPaneTabHoverBackground = UIManager.getColor("TabbedPane.hoverBackground");
        }
        return TabbedPaneTabHoverBackground;
    }

    public Color getTabbedPaneTabAreaBackground() {
        if (TabbedPaneTabAreaBackground == null) {
            TabbedPaneTabAreaBackground = UIManager.getColor("TabbedPane.tabAreaBackground");
        }
        return TabbedPaneTabAreaBackground;
    }

    public Color getTabbedPaneSelectedForeground() {
        if (TabbedPaneSelectedForeground == null) {
            TabbedPaneSelectedForeground = UIManager.getColor("TabbedPane.selectedForeground");
        }
        return TabbedPaneSelectedForeground;
    }

    //TaskPane

    public Color getTaskPaneBorderColor() {
        if (TaskPaneBorderColor == null) {
            TaskPaneBorderColor = UIManager.getColor("TaskPane.borderColor");
        }
        return TaskPaneBorderColor;
    }

    public Color getTaskPaneTitleBorderColor() {
        if (TaskPaneTitleBorderColor == null) {
            TaskPaneTitleBorderColor = UIManager.getColor("TaskPane.titleBorderColor");
        }
        return TaskPaneTitleBorderColor;
    }


    //ToolBar

    public Color getToolBarDropBackground() {
        if (ToolBarDropBackground == null) {
            ToolBarDropBackground = UIManager.getColor("ToolBar.dropColor");
        }
        return ToolBarDropBackground;
    }

    public Color getToolBarBackground() {
        if (ToolBarBackground == null) {
            ToolBarBackground = UIManager.getColor("ToolBar.background");
        }
        return ToolBarBackground;
    }

    public Color getToolBarDockingBackground() {
        if (ToolBarDockingBackground == null) {
            ToolBarDockingBackground = UIManager.getColor("ToolBar.dockingBackground");
        }
        return ToolBarDockingBackground;
    }

    public Color getToolBarFloatingBackground() {
        if (ToolBarFloatingBackground == null) {
            ToolBarFloatingBackground = UIManager.getColor("ToolBar.floatingBackground");
        }
        return ToolBarFloatingBackground;
    }

    public Color getToolBarDockingBorderColor() {
        if (ToolBarDockingBorderColor == null) {
            ToolBarDockingBorderColor = UIManager.getColor("ToolBar.dockingForeground");
        }
        return ToolBarDockingBorderColor;
    }

    public Color getToolBarFloatingBorderColor() {
        if (ToolBarFloatingBorderColor == null) {
            ToolBarFloatingBorderColor = UIManager.getColor("ToolBar.floatingForeground");
        }
        return ToolBarFloatingBorderColor;
    }


    //List

    public Color getListFocusBorderColor() {
        if (ListFocusBorderColor == null) {
            ListFocusBorderColor = UIManager.getColor("List.focusBorderColor");
        }
        return ListFocusBorderColor;
    }

    //TabFrame

    public Color getTabFrameTabForeground() {
        if (TabFrameTabForeground == null) {
            TabFrameTabForeground = UIManager.getColor("TabFrameTab.foreground");
        }
        return TabFrameTabForeground;
    }

    public Color getTabFrameTabSelectedBackground() {
        if (TabFrameTabSelectedBackground == null) {
            TabFrameTabSelectedBackground = UIManager.getColor("TabFrameTab.selectedBackground");
        }
        return TabFrameTabSelectedBackground;
    }

    public Color getTabFrameTabHoverBackground() {
        if (TabFrameTabHoverBackground == null) {
            TabFrameTabHoverBackground = UIManager.getColor("TabFrameTab.hoverBackground");
        }
        return TabFrameTabHoverBackground;
    }

    public Color getTabFrameTabSelectedForeground() {
        if (TabFrameTabSelectedForeground == null) {
            TabFrameTabSelectedForeground = UIManager.getColor("TabFrameTab.selectedForeground");
        }
        return TabFrameTabSelectedForeground;
    }

    public Color getTabFramePopupHeaderHoverBackground() {
        if (TabFramePopupHeaderHoverBackground == null) {
            TabFramePopupHeaderHoverBackground = UIManager.getColor("TabFramePopup.headerHoverBackground");
        }
        return TabFramePopupHeaderHoverBackground;
    }

    public Color getTabFramePopupHeaderSelectedBackground() {
        if (TabFramePopupHeaderSelectedBackground == null) {
            TabFramePopupHeaderSelectedBackground = UIManager.getColor("TabFramePopup.headerSelectedBackground");
        }
        return TabFramePopupHeaderSelectedBackground;
    }

    public Color getTabFramePopupHeaderSelectedHoverBackground() {
        if (TabFramePopupHeaderSelectedHoverBackground == null) {
            TabFramePopupHeaderSelectedHoverBackground = UIManager.getColor("TabFramePopup.headerSelectedHoverBackground");
        }
        return TabFramePopupHeaderSelectedHoverBackground;
    }

    public Color getTabFramePopupHeaderFocusHoverBackground() {
        if (TabFramePopupHeaderFocusHoverBackground == null) {
            TabFramePopupHeaderFocusHoverBackground = UIManager.getColor("TabFramePopup.headerFocusHoverBackground");
        }
        return TabFramePopupHeaderFocusHoverBackground;
    }

    public Color getTabFramePopupHeaderFocusSelectedBackground() {
        if (TabFramePopupHeaderFocusSelectedBackground == null) {
            TabFramePopupHeaderFocusSelectedBackground = UIManager.getColor("TabFramePopup.headerFocusSelectedBackground");
        }
        return TabFramePopupHeaderFocusSelectedBackground;
    }

    public Color getTabFramePopupHeaderFocusSelectedHoverBackground() {
        if (TabFramePopupHeaderFocusSelectedHoverBackground == null) {
            TabFramePopupHeaderFocusSelectedHoverBackground = UIManager.getColor("TabFramePopup.headerFocusSelectedHoverBackground");
        }
        return TabFramePopupHeaderFocusSelectedHoverBackground;
    }

    public Color getTabFramePopupHeaderBackground() {
        if (TabFramePopupHeaderBackground == null) {
            TabFramePopupHeaderBackground = UIManager.getColor("TabFramePopup.headerBackground");
        }
        return TabFramePopupHeaderBackground;
    }

    public Color getTabFramePopupButtonHoverBackground() {
        if (TabFramePopupButtonHoverBackground == null) {
            TabFramePopupButtonHoverBackground = UIManager.getColor("TabFramePopup.headerButtonHoverBackground");
        }
        return TabFramePopupButtonHoverBackground;
    }

    public Color getTabFramePopupButtonClickBackground() {
        if (TabFramePopupButtonClickBackground == null) {
            TabFramePopupButtonClickBackground = UIManager.getColor("TabFramePopup.headerButtonClickBackground");
        }
        return TabFramePopupButtonClickBackground;
    }

    public Color getTabFramePopupFocusHeaderBackground() {
        if (TabFramePopupFocusHeaderBackground == null) {
            TabFramePopupFocusHeaderBackground = UIManager.getColor("TabFramePopup.headerFocusBackground");
        }
        return TabFramePopupFocusHeaderBackground;
    }

    public Color getTabFramePopupButtonFocusHoverBackground() {
        if (TabFramePopupButtonFocusHoverBackground == null) {
            TabFramePopupButtonFocusHoverBackground = UIManager.getColor("TabFramePopup.headerButtonFocusHoverBackground");
        }
        return TabFramePopupButtonFocusHoverBackground;
    }

    public Color getTabFramePopupButtonFocusClickBackground() {
        if (TabFramePopupButtonFocusClickBackground == null) {
            TabFramePopupButtonFocusClickBackground = UIManager.getColor("TabFramePopup.headerButtonFocusClickBackground");
        }
        return TabFramePopupButtonFocusClickBackground;
    }

    public Color getTabFramePopupBorderColor() {
        if (TabFramePopupBorderColor == null) {
            TabFramePopupBorderColor = UIManager.getColor("TabFramePopup.borderColor");
        }
        return TabFramePopupBorderColor;
    }

    public Color getTabFrameBorderColor() {
        if (TabFrameBorderColor == null) {
            TabFrameBorderColor = UIManager.getColor("TabFrame.line");
        }
        return TabFrameBorderColor;
    }

    //FileChooser

    public Color getFileChooserListViewBorderColor() {
        if (FileChooserListViewBorderColor == null) {
            FileChooserListViewBorderColor = UIManager.getColor("FileChooser.borderColor");
        }
        return FileChooserListViewBorderColor;
    }

    public Color getFilePaneListViewBackground() {
        if (FilePaneListViewBackground == null) {
            FilePaneListViewBackground = UIManager.getColor("FileChooser.listViewBackground");
        }
        return FilePaneListViewBackground;
    }

    //Table

    public Color getTableHeaderBorderColor() {
        if (TableHeaderBorderColor == null) {
            TableHeaderBorderColor = UIManager.getColor("TableHeader.borderColor");
        }
        return TableHeaderBorderColor;
    }

    public Color getTableRowBorderColor() {
        if (TableRowBorderColor == null) {
            TableRowBorderColor = UIManager.getColor("Table.focusRowBorderColor");
        }
        return TableRowBorderColor;
    }

    public Color getTableFocusBorderColor() {
        if (TableFocusBorderColor == null) {
            TableFocusBorderColor = UIManager.getColor("Table.focusBorderColor");
        }
        return TableFocusBorderColor;
    }

    public Color getTableHeaderBackground() {
        if (TableHeaderBackground == null) {
            TableHeaderBackground = UIManager.getColor("TableHeader.background");
        }
        return TableHeaderBackground;
    }

    public Color getTableAlternativeBackground() {
        if (TableAlternativeBackground == null) {
            TableAlternativeBackground = UIManager.getColor("Table.alternateRowBackground");
        }
        return TableAlternativeBackground;
    }

    public Color getTableDropLineColor() {
        if (TableDropLineColor == null) {
            TableDropLineColor = UIManager.getColor("Table.dropLineColor");
        }
        return TableDropLineColor;
    }

    public Color getTableDropLineShortColor() {
        if (TableDropLineShortColor == null) {
            TableDropLineShortColor = UIManager.getColor("Table.dropLineShortColor");
        }
        return TableDropLineShortColor;
    }

    public Color getTableBackground() {
        if (TableBackground == null) {
            TableBackground = UIManager.getColor("Table.background");
        }
        return TableBackground;
    }

    public Color getTableFocusSelectionBackground() {
        if (TableFocusSelectionBackground == null) {
            TableFocusSelectionBackground = UIManager.getColor("Table.focusSelectionBackground");
        }
        return TableFocusSelectionBackground;
    }

    public Color getTableSelectionBackground() {
        if (TableSelectionBackground == null) {
            TableSelectionBackground = UIManager.getColor("Table.selectionNoFocusBackground");
        }
        return TableSelectionBackground;
    }

    public Color getTableGridColor() {
        if (TableGridColor == null) {
            TableGridColor = UIManager.getColor("Table.gridColor");
        }
        return TableGridColor;
    }

    //ColorChooser

    public Color getColorChooserPreviewBorderColor() {
        if (ColorChooserPreviewBorderColor == null) {
            ColorChooserPreviewBorderColor = UIManager.getColor("ColorChooser.previewBorderColor");
        }
        return ColorChooserPreviewBorderColor;
    }

    public Color getColorChooserDefaultRecentColor() {
        if (ColorChooserDefaultRecentColor == null) {
            ColorChooserDefaultRecentColor = UIManager.getColor("ColorChooser.swatchesDefaultRecentColor");
        }
        return ColorChooserDefaultRecentColor;
    }

    public Color getColorChooserSliderBorderColor() {
        if (ColorChooserSliderBorderColor == null) {
            ColorChooserSliderBorderColor = UIManager.getColor("ColorChooser.sliderBorderColor");
        }
        return ColorChooserSliderBorderColor;
    }

    public Color getColorChooserSliderShadowColor() {
        if (ColorChooserSliderShadowColor == null) {
            ColorChooserSliderShadowColor = UIManager.getColor("ColorChooser.sliderShadow");
        }
        return ColorChooserSliderShadowColor;
    }

    public Color getColorChooserSliderKnobColor() {
        if (ColorChooserSliderKnobColor == null) {
            ColorChooserSliderKnobColor = UIManager.getColor("ColorChooser.sliderKnobColor");
        }
        return ColorChooserSliderKnobColor;
    }

    public Color getColorChooserColorWheelDropBackground() {
        if (ColorChooserColorWheelDropBackground == null) {
            ColorChooserColorWheelDropBackground = UIManager.getColor("ColorChooser.colorWheelDropBackgroundColor");
        }
        return ColorChooserColorWheelDropBackground;
    }

    public Color getColorChooserColorWheelDropBorderColor() {
        if (ColorChooserColorWheelDropBorderColor == null) {
            ColorChooserColorWheelDropBorderColor = UIManager.getColor("ColorChooser.colorWheelDropBorderColor");
        }
        return ColorChooserColorWheelDropBorderColor;
    }

    public Color getColorChooserSwatchBorderColor() {
        if (ColorChooserSwatchBorderColor == null) {
            ColorChooserSwatchBorderColor = UIManager.getColor("ColorChooser.swatchBorderColor");
        }
        return ColorChooserSwatchBorderColor;
    }

    public Color getColorChooserPipetteBorderColor() {
        if (ColorChooserPipetteBorderColor == null) {
            ColorChooserPipetteBorderColor = UIManager.getColor("ColorChooser.pipetteBorderColor");
        }
        return ColorChooserPipetteBorderColor;
    }

    public Color getColorChooserSwatchGridColor() {
        if (ColorChooserSwatchGridColor == null) {
            ColorChooserSwatchGridColor = UIManager.getColor("ColorChooser.swatchGridColor");
        }
        return ColorChooserSwatchGridColor;
    }

    //Panel

    public Color getPanelBackground() {
        if (PanelBackground == null) {
            PanelBackground = UIManager.getColor("Panel.background");
        }
        return PanelBackground;
    }

    //RadioButton

    public Color getRadioButtonBackground() {
        if (RadioButtonBackground == null) {
            RadioButtonBackground = UIManager.getColor("RadioButton.activeFillColor");
        }
        return RadioButtonBackground;
    }

    public Color getRadioButtonInactiveBackground() {
        if (RadioButtonInactiveBackground == null) {
            RadioButtonInactiveBackground = UIManager.getColor("RadioButton.inactiveFillColor");
        }
        return RadioButtonInactiveBackground;
    }

    public Color getRadioButtonFocusBorderColor() {
        if (RadioButtonFocusBorderColor == null) {
            RadioButtonFocusBorderColor = UIManager.getColor("RadioButton.focusBorderColor");
        }
        return RadioButtonFocusBorderColor;
    }

    public Color getRadioButtonBorderColor() {
        if (RadioButtonBorderColor == null) {
            RadioButtonBorderColor = UIManager.getColor("RadioButton.activeBorderColor");
        }
        return RadioButtonBorderColor;
    }

    public Color getRadioButtonInactiveBorderColor() {
        if (RadioButtonInactiveBorderColor == null) {
            RadioButtonInactiveBorderColor = UIManager.getColor("RadioButton.inactiveBorderColor");
        }
        return RadioButtonInactiveBorderColor;
    }

    public Color getRadioButtonCheckColor() {
        if (RadioButtonCheckColor == null) {
            RadioButtonCheckColor = UIManager.getColor("RadioButton.selectionEnabledColor");
        }
        return RadioButtonCheckColor;
    }

    public Color getRadioButtonCheckInactiveColor() {
        if (RadioButtonCheckInactiveColor == null) {
            RadioButtonCheckInactiveColor = UIManager.getColor("RadioButton.selectionDisabledColor");
        }
        return RadioButtonCheckInactiveColor;
    }

    //DesktopIcon

    public Color getDesktopIconHoverBackground() {
        if (DesktopIconHoverBackground == null) {
            DesktopIconHoverBackground = UIManager.getColor("DesktopIcon.hoverColor");
        }
        return DesktopIconHoverBackground;
    }

    public Color getDesktopIconClickBackground() {
        if (DesktopIconClickBackground == null) {
            DesktopIconClickBackground = UIManager.getColor("DesktopIcon.clickColor");
        }
        return DesktopIconClickBackground;
    }

    public Color getDesktopIconBorderColor() {
        if (DesktopIconBorderColor == null) {
            DesktopIconBorderColor = UIManager.getColor("DesktopIcon.borderColor");
        }
        return DesktopIconBorderColor;
    }

    //InternalFrame

    public Color getInternalFrameBorderShadowColor() {
        if (InternalFrameBorderShadowColor == null) {
            InternalFrameBorderShadowColor = UIManager.getColor("InternalFrame.borderShadowColor");
        }
        return InternalFrameBorderShadowColor;
    }

    public Color getInternalFrameTitleBackground() {
        if (InternalFrameTitleBackground == null) {
            InternalFrameTitleBackground = UIManager.getColor("InternalFrameTitlePane.backgroundColor");
        }
        return InternalFrameTitleBackground;
    }

    public Color getInternalFrameTitleSelectedBackground() {
        if (InternalFrameTitleSelectedBackground == null) {
            InternalFrameTitleSelectedBackground = UIManager.getColor("InternalFrameTitlePane.selectedBackgroundColor");
        }
        return InternalFrameTitleSelectedBackground;
    }

    public Color getInternalFrameTitleSelectedForeground() {
        if (InternalFrameTitleSelectedForeground == null) {
            InternalFrameTitleSelectedForeground = UIManager.getColor("InternalFrameTitlePane.selectedTextForeground");
        }
        return InternalFrameTitleSelectedForeground;
    }

    public Color getInternalFrameTitleForeground() {
        if (InternalFrameTitleForeground == null) {
            InternalFrameTitleForeground = UIManager.getColor("InternalFrameTitlePane.textForeground");
        }
        return InternalFrameTitleForeground;
    }

    public Color getInternalFrameSelectedButtonBackground() {
        if (InternalFrameSelectedButtonBackground == null) {
            InternalFrameSelectedButtonBackground = UIManager.getColor("InternalFrameTitlePane.selectedButtonColor");
        }
        return InternalFrameSelectedButtonBackground;
    }

    public Color getInternalFrameSelectedButtonHoverBackground() {
        if (InternalFrameSelectedButtonHoverBackground == null) {
            InternalFrameSelectedButtonHoverBackground = UIManager.getColor("InternalFrameTitlePane.selectedButtonHoverColor");
        }
        return InternalFrameSelectedButtonHoverBackground;
    }

    public Color getInternalFrameSelectedButtonClickBackground() {
        if (InternalFrameSelectedButtonClickBackground == null) {
            InternalFrameSelectedButtonClickBackground = UIManager.getColor("InternalFrameTitlePane.selectedButtonClickColor");
        }
        return InternalFrameSelectedButtonClickBackground;
    }

    public Color getInternalFrameButtonBackground() {
        if (InternalFrameButtonBackground == null) {
            InternalFrameButtonBackground = UIManager.getColor("InternalFrameTitlePane.buttonColor");
        }
        return InternalFrameButtonBackground;
    }

    public Color getInternalFrameButtonHoverBackground() {
        if (InternalFrameButtonHoverBackground == null) {
            InternalFrameButtonHoverBackground = UIManager.getColor("InternalFrameTitlePane.buttonHoverColor");
        }
        return InternalFrameButtonHoverBackground;
    }

    public Color getInternalFrameButtonClickBackground() {
        if (InternalFrameButtonClickBackground == null) {
            InternalFrameButtonClickBackground = UIManager.getColor("InternalFrameTitlePane.buttonClickColor");
        }
        return InternalFrameButtonClickBackground;
    }

    public Color getInternalFrameBorderColor() {
        if (InternalFrameBorderColor == null) {
            InternalFrameBorderColor = UIManager.getColor("InternalFrameTitlePane.borderColor");
        }
        return InternalFrameBorderColor;
    }

    //ToolTip

    public Color getToolTipBorderColor() {
        if (ToolTipBorderColor == null) {
            ToolTipBorderColor = UIManager.getColor("ToolTip.borderColor");
        }
        return ToolTipBorderColor;
    }

    public Color getToolTipShadowColor() {
        if (ToolTipShadowColor == null) {
            ToolTipShadowColor = UIManager.getColor("ToolTip.borderShadowColor");
        }
        return ToolTipShadowColor;
    }

    //ProgressBar

    public Color getProgressBarTrackBackground() {
        if (ProgressBarTrackBackground == null) {
            ProgressBarTrackBackground = UIManager.getColor("ProgressBar.trackColor");
        }
        return ProgressBarTrackBackground;
    }

    public Color getProgressBarProgressColor() {
        if (ProgressBarProgressColor == null) {
            ProgressBarProgressColor = UIManager.getColor("ProgressBar.progressColor");
        }
        return ProgressBarProgressColor;
    }

    public Color getProgressBarIndeterminateStartColor() {
        if (ProgressBarIndeterminateStartColor == null) {
            ProgressBarIndeterminateStartColor = UIManager.getColor("ProgressBar.indeterminateStartColor");
        }
        return ProgressBarIndeterminateStartColor;
    }

    public Color getProgressBarIndeterminateEndColor() {
        if (ProgressBarIndeterminateEndColor == null) {
            ProgressBarIndeterminateEndColor = UIManager.getColor("ProgressBar.indeterminateEndColor");
        }
        return ProgressBarIndeterminateEndColor;
    }

    public Color getProgressBarFailedColor() {
        if (ProgressBarFailedColor == null) {
            ProgressBarFailedColor = UIManager.getColor("ProgressBar.failedColor");
        }
        return ProgressBarFailedColor;
    }

    public Color getProgressBarFailedEndColor() {
        if (ProgressBarFailedEndColor == null) {
            ProgressBarFailedEndColor = UIManager.getColor("ProgressBar.failedEndColor");
        }
        return ProgressBarFailedEndColor;
    }

    public Color getProgressBarPassedColor() {
        if (ProgressBarPassedColor == null) {
            ProgressBarPassedColor = UIManager.getColor("ProgressBar.passedColor");
        }
        return ProgressBarPassedColor;
    }

    public Color getProgressBarPassedEndColor() {
        if (ProgressBarPassedEndColor == null) {
            ProgressBarPassedEndColor = UIManager.getColor("ProgressBar.passedEndColor");
        }
        return ProgressBarPassedEndColor;
    }

    //Glow

    public Color getErrorGlow() {
        if (ErrorGlow == null) {
            ErrorGlow = UIManager.getColor("glowError");
        }
        return ErrorGlow;
    }

    public Color getErrorFocusGlow() {
        if (ErrorFocusGlow == null) {
            ErrorFocusGlow = UIManager.getColor("glowFocusError");
        }
        return ErrorFocusGlow;
    }

    public Color getFocusGlow() {
        if (FocusGlow == null) {
            FocusGlow = UIManager.getColor("glowFocus");
        }
        return FocusGlow;
    }

    public Color getWarningGlow() {
        if (WarningGlow == null) {
            WarningGlow = UIManager.getColor("glowWarning");
        }
        return WarningGlow;
    }
}
