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
package com.github.weisj.darklaf.graphics;

public class DefaultInterpolator {

    public static final Interpolator LINEAR = f -> f;
    public static final Interpolator LINEAR_REVERSE = f -> 1 - f;
    public static final Interpolator EASE = new CubicBezierEasingInterpolator(0.25f, 0.1f, 0.25f, 1f);
    public static final Interpolator EASE_IN = new CubicBezierEasingInterpolator(0.42f, 0f, 1f, 1f);
    public static final Interpolator EASE_IN_OUT = new CubicBezierEasingInterpolator(0.42f, 0f, 0.58f, 1f);
    public static final Interpolator EASE_OUT = new CubicBezierEasingInterpolator(0f, 0f, 0.58f, 1f);


    private DefaultInterpolator() {
        throw new IllegalStateException("Utility class");
    }

    private static class CubicBezierEasingInterpolator implements Interpolator {

        private final float x1;
        private final float y1;
        private final float x2;
        private final float y2;

        private CubicBezierEasingInterpolator(final float x1, final float y1, final float x2, final float y2) {
            this.x1 = checkControlPoint(x1);
            this.y1 = checkControlPoint(y1);
            this.x2 = checkControlPoint(x2);
            this.y2 = checkControlPoint(y2);
        }

        private float checkControlPoint(final float p) {
            if (p < 0 || p > 1) {
                throw new IllegalArgumentException("control points must be in range [0, 1]");
            }
            return p;
        }

        @Override
        public float interpolate(final float fraction) {
            if (fraction <= 0) return 0;
            if (fraction >= 1) return 1;

            // use binary search
            float low = 0;
            float high = 1;
            while (true) {
                float mid = (low + high) / 2;
                float estimate = cubicBezier(mid, x1, x2);
                if (Math.abs(fraction - estimate) < 0.0005f) return cubicBezier(mid, y1, y2);
                if (estimate < fraction)
                    low = mid;
                else
                    high = mid;
            }
        }

        /**
         * Computes the x or y point on a cubic bezier curve for a given t value.
         * <p>
         * https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Cubic_B%C3%A9zier_curves
         * <p>
         * The general cubic bezier formula is:
         *
         * <pre>
         *   x = b0*x0 + b1*x1 + b2*x2 + b3*x3
         *   y = b0*y0 + b1*y1 + b2*y2 + b3*y3
         * </pr>
         * <p>
         * where:
         * <pre>
         *   b0 = (1-t)^3
         *   b1 = 3 * t * (1-t)^2
         *   b2 = 3 * t^2 * (1-t)
         *   b3 = t^3
         * </pr>
         *
         *  x0,y0 is always 0,0 and x3,y3 is 1,1, so we can simplify to:
         *
         * <pre>
         *   x = b1*x1 + b2*x2 + b3
         *   y = b1*x1 + b2*x2 + b3
         * </pre>
         */
        private static float cubicBezier(final float t, final float xy1, final float xy2) {
            float invT = (1 - t);
            float b1 = 3 * t * (invT * invT);
            float b2 = 3 * (t * t) * invT;
            float b3 = t * t * t;
            return (b1 * xy1) + (b2 * xy2) + b3;
        }
    }
}
