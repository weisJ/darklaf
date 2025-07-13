/*
 * MIT License
 *
 * Copyright (c) 2023-2025 Jannis Weis
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
package com.github.weisj.darklaf;

import java.awt.*;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.swing.*;

import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.graphics.SizedPainter;
import com.github.weisj.darklaf.platform.DecorationsConstants;
import com.github.weisj.darklaf.util.AlignmentExt;

public class Customization {

    @Retention(RetentionPolicy.SOURCE)
    @Target(ElementType.FIELD)
    @interface Key {
        Class<?> valueType();

        String defaultValue() default "";

        String detail() default "";
    }
    public interface Button {
        @Key(valueType = String.class, detail = "One of the values starting with VARIANT_")
        String KEY_VARIANT = "JButton.variant";
        String VARIANT_BORDERLESS_RECTANGULAR = "borderlessRectangular";
        String VARIANT_BORDERLESS = "borderless";
        String VARIANT_NONE = "none";

        @Key(valueType = Color.class)
        String KEY_HOVER_COLOR = "JButton.borderless.hover";
        @Key(valueType = Color.class)
        String KEY_CLICK_COLOR = "JButton.borderless.click";

        @Key(valueType = Boolean.class)
        String KEY_ALT_ARC = "JButton.alternativeArc";
        @Key(valueType = Boolean.class)
        String KEY_NO_ARC = "JButton.noArc";
        @Key(valueType = Integer.class)
        String KEY_ARC_MULTIPLIER = "JButton.arcMultiplier";
        @Key(valueType = Boolean.class)
        String KEY_SQUARE = "JButton.square";
        @Key(valueType = Boolean.class)
        String KEY_ROUND = "JButton.round";
        @Key(valueType = Boolean.class)
        String KEY_THIN = "JButton.thin";

        @Key(valueType = Boolean.class)
        String KEY_NO_BORDERLESS_OVERWRITE = "JButton.noBorderlessOverwrite";

        @Key(valueType = AlignmentExt.class)
        String KEY_CORNER = "JButton.cornerFlag";

        @Key(valueType = JComponent.class)
        String KEY_LEFT_NEIGHBOUR = "JButton.leftNeighbour";
        @Key(valueType = JComponent.class)
        String KEY_RIGHT_NEIGHBOUR = "JButton.rightNeighbour";
        @Key(valueType = JComponent.class)
        String KEY_TOP_NEIGHBOUR = "JButton.topNeighbour";
        @Key(valueType = JComponent.class)
        String KEY_TOP_LEFT_NEIGHBOUR = "JButton.topLeftNeighbour";
        @Key(valueType = JComponent.class)
        String KEY_TOP_RIGHT_NEIGHBOUR = "JButton.topRightNeighbour";
        @Key(valueType = JComponent.class)
        String KEY_BOTTOM_NEIGHBOUR = "JButton.bottomNeighbour";
        @Key(valueType = JComponent.class)
        String KEY_BOTTOM_LEFT_NEIGHBOUR = "JButton.bottomLeftNeighbour";
        @Key(valueType = JComponent.class)
        String KEY_BOTTOM_RIGHT_NEIGHBOUR = "JButton.bottomRightNeighbour";
    }

    public interface ComboBox {
        @Key(valueType = Boolean.class,
                detail = "Stop combobox firing model change events while scrolling in the popup")
        String KEY_DO_NOT_UPDATE_WHEN_SCROLLED = "JComboBox.isTableCellEditor";
    }

    public interface Cell {
        String RENDER_TYPE_CHECKBOX = "checkBox";
        String RENDER_TYPE_RADIOBUTTON = "radioButton";
    }

    public interface Table {
        @Key(valueType = Boolean.class)
        String KEY_ALTERNATE_ROW_COLOR = "JTable.alternateRowColor";
        @Key(valueType = Boolean.class)
        String KEY_RENDER_BOOLEAN_AS_CHECKBOX = "JTable.renderBooleanAsCheckBox";
        @Key(valueType = String.class, detail = "Render type values specified in Customization.Cell")
        String KEY_BOOLEAN_RENDER_TYPE = "JTable.booleanRenderType";
    }

    public interface Tree {
        @Key(valueType = Boolean.class)
        String KEY_ALTERNATE_ROW_COLOR = "JTree.alternateRowColor";
        @Key(valueType = Boolean.class)
        String KEY_RENDER_BOOLEAN_AS_CHECKBOX = "JTree.renderBooleanAsCheckBox";
        @Key(valueType = String.class)
        String KEY_BOOLEAN_RENDER_TYPE = "JTree.booleanRenderType";
        @Key(valueType = String.class)
        String KEY_LINE_STYLE = "JTree.lineStyle";
        String STYLE_LINE = "line";
        String STYLE_DASHED = "dashed";
        String STYLE_NONE = "none";
    }

    public interface List {
        @Key(valueType = Boolean.class)
        String KEY_ALTERNATE_ROW_COLOR = "JList.alternateRowColor";
    }

    public interface ProgressBar {
        @Key(valueType = Boolean.class)
        String KEY_FAILED = "JProgressBar.failed";
        @Key(valueType = Boolean.class)
        String KEY_PASSED = "JProgressBar.passed";
    }

    public interface RootPane extends DecorationsConstants {
    }

    public interface ScrollPane {
        @Key(valueType = Boolean.class)
        String KEY_FAST_WHEEL_SCROLLING = "JScrollBar.fastWheelScrolling";
        @Key(valueType = Boolean.class)
        String KEY_HIGHLIGHT_ON_SCROLL = "JScrollBar.highlightOnScroll";
        @Key(valueType = Boolean.class)
        String KEY_SMALL = "JComponent.small";
        @Key(valueType = SizedPainter.class)
        String KEY_BACKGROUND_PAINTER = "JScrollBar.backgroundPainter";
    }

    public interface Slider {
        @Key(valueType = String.class)
        String KEY_VARIANT = "JSlider.variant";
        String VARIANT_VOLUME = "volume";

        @Key(valueType = Boolean.class)
        String KEY_THUMB_ARROW_SHAPE = "JSlider.paintThumbArrowShape";
        @Key(valueType = Boolean.class)
        String KEY_SHOW_VOLUME_ICON = "JSlider.volume.showIcon";
        @Key(valueType = Boolean.class)
        String KEY_INSTANT_SCROLL = "JSlider.instantScrollEnabled";
        @Key(valueType = Boolean.class)
        String KEY_MANUAL_LABEL_ALIGN = "JSlider.manualLabelAlign";
        @Key(valueType = Boolean.class)
        String KEY_USE_TRACK_AS_BASELINE = "JSlider.useTrackAsBaseline";
    }

    public interface Spinner {
        @Key(valueType = String.class)
        String KEY_VARIANT = "JSpinner.variant";
        String VARIANT_PLUS_MINUS = "plusMinus";
    }

    public interface SplitPane {
        @Key(valueType = String.class)
        String KEY_STYLE = "JSplitPane.style";
        String STYLE_GRIP = "grip";
        String STYLE_GRIP_BORDERLESS = "gripBorderless";
        String STYLE_LINE = "line";
        String STYLE_INVISIBLE = "invisible";
    }

    public interface TabbedPane {
        /*
         * Centering tabs only applies if in - WRAP_TAB_LAYOUT there is only one tab run. -
         * SCROLL_TAB_LAYOUT the viewport doesn't need to be scrolled.
         */
        @Key(valueType = Boolean.class)
        String KEY_CENTER_TABS = "JTabbedPane.centerTabs";
        @Key(valueType = Boolean.class)
        String KEY_DND = "JTabbedPane.dndEnabled";

        @Key(valueType = JComponent.class)
        String KEY_NORTH_COMP = "JTabbedPane.northComponent";
        @Key(valueType = JComponent.class)
        String KEY_WEST_COMP = "JTabbedPane.westComponent";
        @Key(valueType = JComponent.class)
        String KEY_EAST_COMP = "JTabbedPane.eastComponent";
        @Key(valueType = JComponent.class)
        String KEY_SOUTH_COMP = "JTabbedPane.southComponent";
        @Key(valueType = JComponent.class)
        String KEY_LEADING_COMP = "JTabbedPane.leadingComponent";
        @Key(valueType = JComponent.class)
        String KEY_TRAILING_COMP = "JTabbedPane.trailingComponent";

        @Key(valueType = Boolean.class)
        String KEY_SHOW_NEW_TAB_BUTTON = "JTabbedPane.showNewTabButton";
        @Key(valueType = Boolean.class)
        String KEY_DRAW_FOCUS_BAR = "JTabbedPane.drawFocusBar";

        @Key(valueType = Insets.class)
        String KEY_CONTENT_BORDER_INSETS = "JTabbedPane.contentBorderInsets";
        @Key(valueType = Insets.class)
        String KEY_TAB_AREA_INSETS = "JTabbedPane.tabAreaInsets";
        @Key(valueType = Action.class)
        String KEY_NEW_TAB_ACTION = "JTabbedPane.newTabAction";
    }

    public interface Text {
        @Key(valueType = Boolean.class)
        String KEY_ROUNDED_SELECTION = "JTextComponent.roundedSelection";
        @Key(valueType = Boolean.class)
        String KEY_EXTEND_LINE_SELECTION = "JTextComponent.extendSelection";
        @Key(valueType = Boolean.class)
        String KEY_HAS_ERROR = "JTextComponent.hasError";
        @Key(valueType = Boolean.class)
        String KEY_HAS_WARNING = "JTextComponent.hasWarning";
        @Key(valueType = String.class)
        String KEY_DEFAULT_TEXT = "JTextComponent.defaultText";
    }

    public interface ToggleButton {
        @Key(valueType = String.class)
        String KEY_VARIANT = "JToggleButton.variant";
        String VARIANT_SLIDER = "slider";
    }

    public interface ToolBar {
        @Key(valueType = Boolean.class,
                defaultValue = "true",
                detail = "Use the background of the toolbar for painting the drop hint")
        String KEY_USE_TOOL_BAR_BACKGROUND = "JToolBar.drag.useToolbarBackground";
    }

    public interface ToolTip {
        @Key(valueType = String.class)
        String KEY_STYLE = "JToolTip.style";
        String VARIANT_PLAIN = "plain";
        String VARIANT_BALLOON = "balloon";
        String VARIANT_PLAIN_BALLOON = "plainBalloon";


        @Key(valueType = Integer.class)
        String KEY_POINTER_WIDTH = "JToolTip.pointerWidth";
        @Key(valueType = Integer.class)
        String KEY_POINTER_HEIGHT = "JToolTip.pointerHeight";
        @Key(valueType = ToolTipContext.class)
        String KEY_CONTEXT = "JToolTip.toolTipContext";
    }
}
