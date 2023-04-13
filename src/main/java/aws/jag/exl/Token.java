/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.exl;

import java.util.*;


public class Token {
    private static final int identifierType = 0;
    private static final int stringType = 1;
    private static final int numberType = 2;
    private static int typeSequence = numberType;
    private final String body;
    private final int type;
    private final Number num;
    private Token(String b, int t, Number n) { body = b; type = t; num = n; }
    @Override public String toString() { return type+":"+(getBody()!=null ? getBody() : getNum()); }
    private static Token of(String s, int t) {
        return cache.computeIfAbsent(s, S->new Token(S, t, null));
    }
    public static Token string(String s) {
        return new Token(s, stringType, null);
    }
    public static Token identifier(String s) {
        return of(s, identifierType);
    }
    public static Token keyword(String s) {
        return of(s, ++typeSequence);
    }
    public static Token number(Number n) {
        return new Token(null, numberType, n);
    }
    private static final Map<String,Token> cache = new HashMap<>();
    public String getBody() {
        return body;
    }
    public int getType() {
        return type;
    }
    public Number getNum() {
        return num;
    }
    public boolean isIdentifier() { return type==identifierType; }
    public boolean isString() { return type==stringType; }
    public boolean isNumber() { return type==numberType; }
    public boolean isKeyword() { return type>numberType; }
}
