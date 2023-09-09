/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

/**
 * Simple <a href="https://en.wikipedia.org/wiki/Glob_(programming)">shell-style
 * glob patterns</a>, essentially simplified regular expressions.
 */
public abstract class Glob {
    private String pattern;
    @Override
    public String toString() {
        return pattern;
    }
    public static Glob compile(String s) {
        var first = s.indexOf('*');
        var last = s.lastIndexOf('*');
        Glob ret;
        if(first < 0)// no stars
            ret = new Glob() {
                @Override
                public String matches(String m) {
                    return s.equals(s) ? "" : null;
                }
            };
        else {
            var head = s.substring(0, first);
            var tail = s.substring(last + 1);
            if(first == last) // exactly one '*'
                if(head.length() == 0)
                    ret = new Glob() { // *Y
                        int lt = tail.length();
                        @Override
                        public String matches(String m) {
                            var len = m.length();
                            return lt <= len
                                   && m.regionMatches(len - lt, tail, 0, lt)
                                    ? substring(m, 0, len - lt) : null;
                        }
                    };
                else if(tail.length() == 0)
                    ret = new Glob() { // X*
                        int lh = head.length();
                        @Override
                        public String matches(String m) {
                            var len = m.length();
                            return lh <= len
                                   && m.regionMatches(0, head, 0, lh) ? substring(m, lh) : null;
                        }
                    };
                else
                    ret = new Glob() { // X*Y
                        int lh = head.length();
                        int lt = tail.length();
                        @Override
                        public String matches(String m) {
                            var len = m.length();
                            return lh + lt <= len
                                   && m.regionMatches(0, head, 0, lh)
                                   && m.regionMatches(len - lt, tail, 0, lt) ? substring(m, lh, len - lt) : null;
                        }
                    };
            else if(first == 0 && last == s.length() - 1)
                ret = new Glob() {  // *X*
                    String body = s.substring(1, last);
                    int lb = body.length();
                    @Override
                    public String matches(String m) {
                        int pos = m.indexOf(body);
                        return pos >= 0 ? substring(m, pos + lb) : null;
                    }
                };
            else ret = new Glob() {
                    @Override
                    public String matches(String m) {
                        throw new UnsupportedOperationException("Complex matches Not supported yet: " + s);
                    }
                };
        }
        ret.pattern = s;
        return ret;
    }
    private static String substring(String m, int st, int end) {
        return m == null
                ? null
                : st == 0 || st >= end || m.charAt(st - 1) != '/' || m.charAt(st)=='/'
                    ? m.substring(st, end)
                    : m.substring(st - 1, end);
    }
    private static String substring(String m, int st) {
        return substring(m, st, m.length());
    }
    public abstract String matches(String s);
}
