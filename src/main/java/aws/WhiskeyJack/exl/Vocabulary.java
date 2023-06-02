/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.exl;


public class Vocabulary {
    public static final Token AND = operator("&");
    public static final Token ANDAND = operator("&&", "\u2227");
    public static final Token ARRAYLITERAL = operator("arraylit:");
    public static final Token ASSIGN = operator("=", "\u2190");
    public static final Token BLOCK = operator("block:");
    public static final Token CASE = Token.keyword("case");
    public static final Token COLON = operator(":");
    public static final Token COMMA = operator(",");
    public static final Token DECLAREASSIGN = operator(":=");
    public static final Token DECLAREASSIGNFINAL = operator(":==");
    public static final Token DEFAULT = Token.keyword("default");
    public static final Token DIVIDE = operator("/", "\u00f7");
    public static final Token DO = Token.keyword("do");
    public static final Token DOT = operator(".");
    public static final Token ELEMENTOF = operator("\u2208"); // element of \u2208
    public static final Token ELSE = Token.keyword("else");
    public static final Token EOF = Token.keyword("-EOF-");
    public static final Token EQ = operator("==", "\u2261");
    public static final Token FALSE = Token.keyword("false");
    public static final Token FOR = operator("for", "\u2200"); // for all \u2200
    public static final Token GE = operator(">=", "\u2265");
    public static final Token GT = operator(">");
    public static final Token IF = Token.keyword("if");
    public static final Token INSTANCEOF = Token.keyword("instanceof");
    public static final Token INVOKE = operator("invoke:");
    public static final Token LBRACE = operator("{");
    public static final Token LE = operator("<=", "\u2264");
    public static final Token LPAREN = operator("(");
    public static final Token LSQUARE = operator("[");
    public static final Token LT = operator("<");
    public static final Token MAPLIT = operator("map:");
    public static final Token MINUS = operator("-", "\u2212"); // minus sign \u2212
    public static final Token MULTIPLY = operator("*", "\u00d7");
    public static final Token NE = operator("!=", "\u2260");
    public static final Token NEW = Token.keyword("new");
    public static final Token NOT = operator("!", "\u00ac");
    public static final Token NULL = operator("null", "\u2205"); // empty set \u2205
    public static final Token OR = operator("|");
    public static final Token OROR = operator("||", "\u2228");
    public static final Token PLUS = operator("+");
    public static final Token QUESTION = operator("?");
    public static final Token RARROW = operator("->", "\u2192", "\u21d2");
    public static final Token RBRACE = operator("}");
    public static final Token RETURN = operator("return", "\u2234"); // therefore \u2234˙
    public static final Token RPAREN = operator(")");
    public static final Token RSQUARE = operator("]");
    public static final Token SEMI = operator(";");
    public static final Token SUBSCRIPT = operator("subscript:");
    public static final Token SWITCH = Token.keyword("switch");
    public static final Token SlashSlashCOMMENT = operator("//");
    public static final Token SlashStarCOMMENT = operator("/*");
    public static final Token TRUE = Token.keyword("true");
    public static final Token UNKNOWN = operator(" \u00a1unknown!");
    public static final Token WHILE = Token.keyword("while");
    
    
    private static Token operator(String... ops) {
        var token = Token.keyword(ops[0]);
        for(var op: ops)
            if(!Character.isJavaIdentifierStart(op.charAt(0)))
                Tokenizer.define(op).setToken(token);
        return token;
    }
    public static void triggerInit() {}

}
