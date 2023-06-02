/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

/**
 * Simple <a href="https://en.wikipedia.org/wiki/Glob_(programming)">shell-style
 * glob patterns</a>, essentially simplified regular expressions. Only  a single
 * '*' is supported for now.
 */
public abstract class Glob {
    public static Glob compile(String s) {
        var first = s.indexOf('*');
        var last = s.lastIndexOf('*');
        if(first < 0) // no stars
            return new Glob() {
                @Override
                public boolean matches(String m) {
                    return s.equals(s);
                }
            };
        var head = s.substring(0, first);
        var tail = s.substring(last + 1);
        if(first == last) // exactly one '*'
            if(head.length() == 0)
                return new Glob() { // *Y
                    int lt = tail.length();
                    @Override
                    public boolean matches(String m) {
                        var len = m.length();
                        return lt <= len
                               && m.regionMatches(len - lt, tail, 0, lt);
                    }
                };
            else if(tail.length() == 0)
                return new Glob() { // X*
                    int lh = head.length();
                    @Override
                    public boolean matches(String m) {
                        var len = m.length();
                        return lh <= len
                               && m.regionMatches(0, head, 0, lh);
                    }
                };
            else
                return new Glob() { // X*Y
                    int lh = head.length();
                    int lt = tail.length();
                    @Override
                    public boolean matches(String m) {
                        var len = m.length();
                        return lh + lt <= len
                               && m.regionMatches(0, head, 0, lh)
                               && m.regionMatches(len - lt, tail, 0, lt);
                    }
                };
        return new Glob() {
            @Override
            public boolean matches(String m) {
                throw new UnsupportedOperationException("Complex matches Not supported yet: "+s);
            }
        };
    }
    public abstract boolean matches(String s);
}
