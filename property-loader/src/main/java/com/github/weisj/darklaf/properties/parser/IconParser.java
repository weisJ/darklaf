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
package com.github.weisj.darklaf.properties.parser;

import java.awt.Dimension;
import java.util.Collections;
import java.util.List;

import javax.swing.Icon;

import com.github.weisj.darklaf.properties.icons.EmptyIcon;
import com.github.weisj.darklaf.properties.icons.StateIcon;
import com.github.weisj.darklaf.properties.icons.UIAwareIcon;

public class IconParser extends KeyFilteredParser implements Delimiters {

    private static final char MODIFIER_START = '[';
    private static final char MODIFIER_END = ']';
    private static final char MODIFIER_DELIMITER = ',';

    private static final String DUAL_KEY = "dual";
    private static final String AWARE_KEY = "aware";
    private static final String THEMED_KEY = "themed";
    private static final String ICON_EMPTY = "empty";

    public IconParser() {
        super(".icon", "Icon", "Image");
    }

    @Override
    public ParseResult doParse(final ParseResult parseResult, final ParserContext context) {
        if (parseResult.value.startsWith(String.valueOf(LIST_START))
                && parseResult.value.endsWith(String.valueOf(LIST_END))) {
            return parseStateIcon(parseResult, context);
        }
        Dimension dim = new Dimension(-1, -1);
        if (parseResult.value.endsWith(String.valueOf(ARG_END))) {
            List<Integer> dimensions = ParserUtil.parseDelimited(ARG_START, ARG_END, ARG_SEPARATOR, false,
                    PropertyParser.of(Integer::parseInt), Integer.class, parseResult, context);
            if (dimensions.size() != 2) return ParserUtil.error(parseResult, "Invalid dimension.");
            dim.width = dimensions.get(0);
            dim.height = dimensions.get(1);
        }

        List<String> modifiers;
        if (parseResult.value.endsWith(String.valueOf(MODIFIER_END))) {
            modifiers = ParserUtil.parseDelimited(MODIFIER_START, MODIFIER_END, MODIFIER_DELIMITER, false,
                    PropertyParser.of(s -> s), String.class, parseResult, context);

        } else {
            modifiers = Collections.emptyList();
        }
        boolean dual = modifiers.contains(DUAL_KEY);
        boolean aware = modifiers.contains(AWARE_KEY);
        boolean themed = modifiers.contains(THEMED_KEY);
        if (aware && themed) {
            return ParserUtil.error(parseResult,
                    "Modifiers " + AWARE_KEY + " and " + THEMED_KEY + " are mutually exclusive.");
        }

        Icon icon;
        if (ICON_EMPTY.equals(parseResult.value)) {
            icon = EmptyIcon.create(dim.width, dim.height);
        } else if (themed) {
            icon = context.iconResolver.getIcon(parseResult.value, dim.width, dim.height, true);
        } else if (dual || aware) {
            UIAwareIcon awareIcon = context.iconResolver.getUIAwareIcon(parseResult.value, dim.width, dim.height);
            if (dual) {
                icon = awareIcon.getDual();
            } else {
                icon = awareIcon;
            }
        } else {
            icon = context.iconResolver.getIcon(parseResult.value, dim.width, dim.height);
        }
        return ParserUtil.setNonNull(parseResult, icon);
    }

    private static ParseResult parseStateIcon(final ParseResult parseResult, final ParserContext context) {
        return ParserUtil.setNonNull(parseResult,
                new StateIcon(ParserUtil.parseDelimited(LIST_START, LIST_END, LIST_SEPARATOR,
                        Icon.class, parseResult, context)));
    }
}
