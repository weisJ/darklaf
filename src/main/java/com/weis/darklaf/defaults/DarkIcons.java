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

import com.weis.darklaf.icons.UIAwareIcon;

import javax.swing.*;

@SuppressWarnings("WeakerAccess")
public class DarkIcons {

    protected static DarkIcons instance;
    protected Icon SplitPaneDividerLeftOneTouch;
    protected Icon SplitPaneDividerRightOneTouch;
    protected Icon SplitPaneDividerTopOneTouch;
    protected Icon SplitPaneDividerBottomOneTouch;
    protected Icon SplitPaneDividerVerticalSplit;
    protected Icon SplitPaneDividerHorizontalSplit;
    protected Icon PasswordFiledShow;
    protected Icon PasswordFiledShowPressed;
    protected Icon TreeExpandedFocusedSelected;
    protected Icon TreeExpandedSelected;
    protected Icon TreeExpandedFocused;
    protected Icon TreeExpanded;
    protected Icon TreeCollapsedFocusedSelected;
    protected Icon TreeCollapsedSelected;
    protected Icon TreeCollapsedFocused;
    protected Icon TreeCollapsed;
    protected Icon TextFieldClearHover;
    protected Icon TextFieldClear;
    protected Icon TextFieldSearchHistory;
    protected Icon TextFieldSearch;
    protected Icon SliderVolumeLevel0;
    protected Icon SliderVolumeLevel1;
    protected Icon SliderVolumeLevel2;
    protected Icon SliderVolumeLevel3;
    protected Icon SliderVolumeLevel4;
    protected Icon SliderVolumeLevel0Inactive;
    protected Icon SliderVolumeLevel1Inactive;
    protected Icon SliderVolumeLevel2Inactive;
    protected Icon SliderVolumeLevel3Inactive;
    protected Icon SliderVolumeLevel4Inactive;
    protected Icon ColorChooserPipette;
    protected Icon ColorChooserPipetteHover;
    protected Icon TabbedPaneMoreTabs;
    protected Icon TabbedPaneNewTabs;
    protected Icon ToolBarHorizontalGrip;
    protected Icon ToolBarVerticalGrip;
    protected UIAwareIcon SpinnerMathUp;
    protected UIAwareIcon SpinnerMathDown;
    protected UIAwareIcon SpinnerArrowUp;
    protected UIAwareIcon SpinnerArrowDown;

    public static DarkIcons get() {
        if (instance == null) {
            instance = new DarkIcons();
        }
        return instance;
    }

    public static void uninstall() {
        if (instance == null) return;
        instance.uninstallIcons();
        instance = null;
    }

    protected void uninstallIcons() {
        SplitPaneDividerLeftOneTouch = null;
        SplitPaneDividerRightOneTouch = null;
        SplitPaneDividerTopOneTouch = null;
        SplitPaneDividerBottomOneTouch = null;
        SplitPaneDividerVerticalSplit = null;
        SplitPaneDividerHorizontalSplit = null;
        PasswordFiledShow = null;
        PasswordFiledShowPressed = null;
        TreeExpandedFocusedSelected = null;
        TreeExpandedSelected = null;
        TreeExpandedFocused = null;
        TreeExpanded = null;
        TreeCollapsedFocusedSelected = null;
        TreeCollapsedSelected = null;
        TreeCollapsedFocused = null;
        TreeCollapsed = null;
        TextFieldClearHover = null;
        TextFieldClear = null;
        TextFieldSearchHistory = null;
        TextFieldSearch = null;
        SliderVolumeLevel0 = null;
        SliderVolumeLevel1 = null;
        SliderVolumeLevel2 = null;
        SliderVolumeLevel3 = null;
        SliderVolumeLevel4 = null;
        SliderVolumeLevel0Inactive = null;
        SliderVolumeLevel1Inactive = null;
        SliderVolumeLevel2Inactive = null;
        SliderVolumeLevel3Inactive = null;
        SliderVolumeLevel4Inactive = null;
        ColorChooserPipette = null;
        ColorChooserPipetteHover = null;
        TabbedPaneMoreTabs = null;
        TabbedPaneNewTabs = null;
        ToolBarHorizontalGrip = null;
        ToolBarVerticalGrip = null;
        SpinnerMathUp = null;
        SpinnerMathDown = null;
        SpinnerArrowUp = null;
        SpinnerArrowDown = null;
    }

    public Icon getSplitPaneDividerLeftOneTouch() {
        if (SplitPaneDividerLeftOneTouch == null) {
            SplitPaneDividerLeftOneTouch = UIManager.getIcon("SplitPaneDivider.leftOneTouch.icon");
        }
        return SplitPaneDividerLeftOneTouch;
    }

    public Icon getSplitPaneDividerRightOneTouch() {
        if (SplitPaneDividerRightOneTouch == null) {
            SplitPaneDividerRightOneTouch = UIManager.getIcon("SplitPaneDivider.rightOneTouch.icon");
        }
        return SplitPaneDividerRightOneTouch;
    }

    public Icon getSplitPaneDividerTopOneTouch() {
        if (SplitPaneDividerTopOneTouch == null) {
            SplitPaneDividerTopOneTouch = UIManager.getIcon("SplitPaneDivider.topOneTouch.icon");
        }
        return SplitPaneDividerTopOneTouch;
    }

    public Icon getSplitPaneDividerBottomOneTouch() {
        if (SplitPaneDividerBottomOneTouch == null) {
            SplitPaneDividerBottomOneTouch = UIManager.getIcon("SplitPaneDivider.bottomOneTouch.icon");
        }
        return SplitPaneDividerBottomOneTouch;
    }

    public Icon getSplitPaneDividerVerticalSplit() {
        if (SplitPaneDividerVerticalSplit == null) {
            SplitPaneDividerVerticalSplit = UIManager.getIcon("SplitPane.verticalGlue.icon");
        }
        return SplitPaneDividerVerticalSplit;
    }

    public Icon getSplitPaneDividerHorizontalSplit() {
        if (SplitPaneDividerHorizontalSplit == null) {
            SplitPaneDividerHorizontalSplit = UIManager.getIcon("SplitPane.horizontalGlue.icon");
        }
        return SplitPaneDividerHorizontalSplit;
    }

    public Icon getPasswordFiledShow() {
        if (PasswordFiledShow == null) {
            PasswordFiledShow = UIManager.getIcon("PasswordField.show.icon");
        }
        return PasswordFiledShow;
    }

    public Icon getPasswordFiledShowPressed() {
        if (PasswordFiledShowPressed == null) {
            PasswordFiledShowPressed = UIManager.getIcon("PasswordField.showPressed.icon");
        }
        return PasswordFiledShowPressed;
    }

    public Icon getTreeExpandedFocusedSelected() {
        if (TreeExpandedFocusedSelected == null) {
            TreeExpandedFocusedSelected = UIManager.getIcon("Tree.expanded.selected.focused.icon");
        }
        return TreeExpandedFocusedSelected;
    }

    public Icon getTreeExpandedSelected() {
        if (TreeExpandedSelected == null) {
            TreeExpandedSelected = UIManager.getIcon("Tree.expanded.selected.unfocused.icon");
        }
        return TreeExpandedSelected;
    }

    public Icon getTreeExpandedFocused() {
        if (TreeExpandedFocused == null) {
            TreeExpandedFocused = UIManager.getIcon("Tree.expanded.unselected.focused.icon");
        }
        return TreeExpandedFocused;
    }

    public Icon getTreeExpanded() {
        if (TreeExpanded == null) {
            TreeExpanded = UIManager.getIcon("Tree.expanded.unselected.unfocused.icon");
        }
        return TreeExpanded;
    }

    public Icon getTreeCollapsedFocusedSelected() {
        if (TreeCollapsedFocusedSelected == null) {
            TreeCollapsedFocusedSelected = UIManager.getIcon("Tree.collapsed.selected.focused.icon");
        }
        return TreeCollapsedFocusedSelected;
    }

    public Icon getTreeCollapsedSelected() {
        if (TreeCollapsedSelected == null) {
            TreeCollapsedSelected = UIManager.getIcon("Tree.collapsed.selected.unfocused.icon");
        }
        return TreeCollapsedSelected;
    }

    public Icon getTreeCollapsedFocused() {
        if (TreeCollapsedFocused == null) {
            TreeCollapsedFocused = UIManager.getIcon("Tree.collapsed.unselected.focused.icon");
        }
        return TreeCollapsedFocused;
    }

    public Icon getTreeCollapsed() {
        if (TreeCollapsed == null) {
            TreeCollapsed = UIManager.getIcon("Tree.collapsed.unselected.unfocused.icon");
        }
        return TreeCollapsed;
    }

    public Icon getTextFieldClearHover() {
        if (TextFieldClearHover == null) {
            TextFieldClearHover = UIManager.getIcon("TextField.search.clearHover.icon");
        }
        return TextFieldClearHover;
    }

    public Icon getTextFieldClear() {
        if (TextFieldClear == null) {
            TextFieldClear = UIManager.getIcon("TextField.search.clear.icon");
        }
        return TextFieldClear;
    }

    public Icon getTextFieldSearchHistory() {
        if (TextFieldSearchHistory == null) {
            TextFieldSearchHistory = UIManager.getIcon("TextField.search.searchWithHistory.icon");
        }
        return TextFieldSearchHistory;
    }

    public Icon getTextFieldSearch() {
        if (TextFieldSearch == null) {
            TextFieldSearch = UIManager.getIcon("TextField.search.search.icon");
        }
        return TextFieldSearch;
    }

    public Icon getSliderVolumeLevel0() {
        if (SliderVolumeLevel0 == null) {
            SliderVolumeLevel0 = UIManager.getIcon("Slider.volume.enabled_level_0.icon");
        }
        return SliderVolumeLevel0;
    }

    public Icon getSliderVolumeLevel1() {
        if (SliderVolumeLevel1 == null) {
            SliderVolumeLevel1 = UIManager.getIcon("Slider.volume.enabled_level_1.icon");
        }
        return SliderVolumeLevel1;
    }

    public Icon getSliderVolumeLevel2() {
        if (SliderVolumeLevel2 == null) {
            SliderVolumeLevel2 = UIManager.getIcon("Slider.volume.enabled_level_2.icon");
        }
        return SliderVolumeLevel2;
    }

    public Icon getSliderVolumeLevel3() {
        if (SliderVolumeLevel3 == null) {
            SliderVolumeLevel3 = UIManager.getIcon("Slider.volume.enabled_level_3.icon");
        }
        return SliderVolumeLevel3;
    }

    public Icon getSliderVolumeLevel4() {
        if (SliderVolumeLevel4 == null) {
            SliderVolumeLevel4 = UIManager.getIcon("Slider.volume.enabled_level_4.icon");
        }
        return SliderVolumeLevel4;
    }

    public Icon getSliderVolumeLevel0Inactive() {
        if (SliderVolumeLevel0Inactive == null) {
            SliderVolumeLevel0Inactive = UIManager.getIcon("Slider.volume.disabled_level_0.icon");
        }
        return SliderVolumeLevel0Inactive;
    }

    public Icon getSliderVolumeLevel1Inactive() {
        if (SliderVolumeLevel1Inactive == null) {
            SliderVolumeLevel1Inactive = UIManager.getIcon("Slider.volume.disabled_level_1.icon");
        }
        return SliderVolumeLevel1Inactive;
    }

    public Icon getSliderVolumeLevel2Inactive() {
        if (SliderVolumeLevel2Inactive == null) {
            SliderVolumeLevel2Inactive = UIManager.getIcon("Slider.volume.disabled_level_2.icon");
        }
        return SliderVolumeLevel2Inactive;
    }

    public Icon getSliderVolumeLevel3Inactive() {
        if (SliderVolumeLevel3Inactive == null) {
            SliderVolumeLevel3Inactive = UIManager.getIcon("Slider.volume.disabled_level_3.icon");
        }
        return SliderVolumeLevel3Inactive;
    }

    public Icon getSliderVolumeLevel4Inactive() {
        if (SliderVolumeLevel4Inactive == null) {
            SliderVolumeLevel4Inactive = UIManager.getIcon("Slider.volume.disabled_level_4.icon");
        }
        return SliderVolumeLevel4Inactive;
    }

    public Icon getColorChooserPipette() {
        if (ColorChooserPipette == null) {
            ColorChooserPipette = UIManager.getIcon("ColorChooser.pipette.icon");
        }
        return ColorChooserPipette;
    }

    public Icon getColorChooserPipetteHover() {
        if (ColorChooserPipetteHover == null) {
            ColorChooserPipetteHover = UIManager.getIcon("ColorChooser.pipetteRollover.icon");
        }
        return ColorChooserPipetteHover;
    }

    public UIAwareIcon getSpinnerArrowDown() {
        if (SpinnerArrowDown == null) {
            SpinnerArrowDown = (UIAwareIcon) UIManager.getIcon("ArrowButton.down.icon");
        }
        return SpinnerArrowDown;
    }

    public UIAwareIcon getSpinnerArrowUp() {
        if (SpinnerArrowUp == null) {
            SpinnerArrowUp = (UIAwareIcon) UIManager.getIcon("ArrowButton.up.icon");
        }
        return SpinnerArrowUp;
    }

    public UIAwareIcon getSpinnerMathDown() {
        if (SpinnerMathDown == null) {
            SpinnerMathDown = (UIAwareIcon) UIManager.getIcon("Spinner.minus.icon");
        }
        return SpinnerMathDown;
    }

    public UIAwareIcon getSpinnerMathUp() {
        if (SpinnerMathUp == null) {
            SpinnerMathUp = (UIAwareIcon) UIManager.getIcon("Spinner.plus.icon");
        }
        return SpinnerMathUp;
    }

    public Icon getTabbedPaneMoreTabs() {
        if (TabbedPaneMoreTabs == null) {
            TabbedPaneMoreTabs = UIManager.getIcon("TabbedPane.moreTabs.icon");
        }
        return TabbedPaneMoreTabs;
    }

    public Icon getTabbedPaneNewTabs() {
        if (TabbedPaneNewTabs == null) {
            TabbedPaneNewTabs = UIManager.getIcon("TabbedPane.newTab.icon");
        }
        return TabbedPaneNewTabs;
    }

    public Icon getToolBarHorizontalGrip() {
        if (ToolBarHorizontalGrip == null) {
            ToolBarHorizontalGrip = UIManager.getIcon("ToolBar.horizontalGrip.icon");
        }
        return ToolBarHorizontalGrip;
    }

    public Icon getToolBarVerticalGrip() {
        if (ToolBarVerticalGrip == null) {
            ToolBarVerticalGrip = UIManager.getIcon("ToolBar.verticalGrip.icon");
        }
        return ToolBarVerticalGrip;
    }
}

