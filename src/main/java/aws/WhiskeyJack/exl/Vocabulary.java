/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.exl;

import aws.WhiskeyJack.nodegraph.*;
import java.util.*;


public class Vocabulary {
    public static final Token AND = operator("&");
    public static final Token ANDAND = operator("&&", "\u2227"); // ∧
    public static final Token ARRAYLITERAL = operator("arraylit:");
    public static final Token ASSIGN = operator("=", "\u2190"); // ←
    public static final Token BLOCK = operator("block:");
    public static final Token CASE = Token.keyword("cast:");
    public static final Token CAST = Token.keyword("case");
    public static final Token COLON = operator(":");
    public static final Token COMMA = operator(",");
    public static final Token DECLARE = operator("var");
    public static final Token DECLAREASSIGN = operator(":=");
    public static final Token DECLAREASSIGNFINAL = operator(":==");
    public static final Token DEFAULT = Token.keyword("default");
    public static final Token DIVIDE = operator("/", "\u00f7");
    public static final Token DO = Token.keyword("do");
    public static final Token DOT = operator(".");
    public static final Token ELSE = Token.keyword("else");
    public static final Token EOF = Token.keyword("-EOF-");
    public static final Token EQ = operator("==", "\u2261"); // ≡
    public static final Token FALSE = Token.keyword("false");
    public static final Token FOR = operator("for", "\u2200"); // for all ∀ \u2200
    public static final Token FUNCTION = operator("defun:");
    public static final Token GE = operator(">=", "\u2265"); // ≥
    public static final Token GT = operator(">");
    public static final Token IF = Token.keyword("if");
    public static final Token INSTANCEOF = operator("instanceof","\u220A", "\u2208"); // ∊, ∈
    public static final Token INVOKE = operator("invoke:");
    public static final Token LBRACE = operator("{");
    public static final Token LE = operator("<=", "\u2264"); // ≤
    public static final Token LPAREN = operator("(");
    public static final Token LSQUARE = operator("[");
    public static final Token LT = operator("<");
    public static final Token MAPLIT = operator("map:");
    public static final Token MINUS = operator("-", "\u2212"); // minus sign \u2212
    public static final Token MULTIPLY = operator("*", "\u00d7"); // ×
    public static final Token NE = operator("!=", "\u2260"); // ≠
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
    public static final Token TYPE = Token.keyword("type:");
    public static final Token UNKNOWN = operator(" \u00a1unknown!");
    public static final Token WHILE = Token.keyword("while");
    private static final boolean[] flatten;
    private static final int[] priority;
    private static final boolean[] rightAssoc;
    private static final Type[] likelyType;
    
    public static int getPriority(Token op) {
        return priority[op.getKind()];
    }
    public static int[] getStandardPriorityTable() { return priority; }
    public static boolean canFlatten(Token op) {
        return flatten[op.getKind()];
    }
    public static boolean isRightAssoc(Token op) {
        return rightAssoc[op.getKind()];
    }
    public static Type likelyType(Token op) {
        return likelyType[op.getKind()];
    }
    private static void initOpProperties() {}
    static {{
            int[] p = new int[Token.kindTableSize()];
            int v = 1;
            Arrays.fill(p, 0, Token.numberKind+1, 99);
            v++;
            p[RARROW.getKind()] = v;
            v++;
            p[ASSIGN.getKind()] = v;
            p[DECLAREASSIGN.getKind()] = v;
            p[DECLAREASSIGNFINAL.getKind()] = v;
            v++;
            p[QUESTION.getKind()] = v;
            p[COLON.getKind()] = v;
            v++;
            p[ANDAND.getKind()] = v;
            p[AND.getKind()] = v;
            p[OR.getKind()] = v;
            p[OROR.getKind()] = v;
            v++;
            p[INSTANCEOF.getKind()] = v;
            p[LT.getKind()] = v;
            p[LE.getKind()] = v;
            p[GT.getKind()] = v;
            p[GE.getKind()] = v;
            p[EQ.getKind()] = v;
            p[NE.getKind()] = v;
            v++;
            p[PLUS.getKind()] = v;
            p[MINUS.getKind()] = v;
            v++;
            p[DIVIDE.getKind()] = v;
            p[MULTIPLY.getKind()] = v;
            v++;
            p[INVOKE.getKind()] = v;
            p[BLOCK.getKind()] = v;
            p[DOT.getKind()] = v;
            priority = p;
        }
        {
            boolean[] p = new boolean[Token.kindTableSize()];
            p[ASSIGN.getKind()] = true;
            p[DECLAREASSIGN.getKind()] = true;
            p[DECLAREASSIGNFINAL.getKind()] = true;
            rightAssoc = p;
        }
        {
            boolean[] p = new boolean[Token.kindTableSize()];
            p[COMMA.getKind()] = true;
            p[SEMI.getKind()] = true;
            p[ANDAND.getKind()] = true;
            p[AND.getKind()] = true;
            p[OR.getKind()] = true;
            p[OROR.getKind()] = true;
            p[PLUS.getKind()] = true;
            p[MULTIPLY.getKind()] = true;
            flatten = p;
        }
        {
            var p = new Type[Token.kindTableSize()];
            Arrays.fill(p, Type.unknown);
            p[Vocabulary.NOT.getKind()] = Type.bool;
            p[Vocabulary.OR.getKind()] = Type.bool;
            p[Vocabulary.OROR.getKind()] = Type.bool;
            p[Vocabulary.INSTANCEOF.getKind()] = Type.bool;
            p[Vocabulary.LT.getKind()] = Type.bool;
            p[Vocabulary.LE.getKind()] = Type.bool;
            p[Vocabulary.GT.getKind()] = Type.bool;
            p[Vocabulary.GE.getKind()] = Type.bool;
            p[Vocabulary.EQ.getKind()] = Type.bool;
            p[Vocabulary.NE.getKind()] = Type.bool;
            p[Vocabulary.ANDAND.getKind()] = Type.bool;
            p[Vocabulary.AND.getKind()] = Type.bool;
            p[Vocabulary.OR.getKind()] = Type.bool;
            p[Vocabulary.OROR.getKind()] = Type.bool;
            p[Vocabulary.TRUE.getKind()] = Type.bool;
            p[Vocabulary.FALSE.getKind()] = Type.bool;
            p[PLUS.getKind()] = Type.number;
            p[MINUS.getKind()] = Type.number;
            p[DIVIDE.getKind()] = Type.number;
            p[MULTIPLY.getKind()] = Type.number;
            p[NEW.getKind()] = Type.tuple;
            likelyType = p;
        }
    }
    
    
    private static Token operator(String... ops) {
        var token = Token.keyword(ops[0]);
        for(var op: ops)
            if(!Character.isJavaIdentifierStart(op.charAt(0)))
                Tokenizer.define(op).setToken(token);
        return token;
    }
    public static final Expression finalMarker = Expression.of(NULL);
    public static final Expression varMarker = Expression.of(NULL);
    static { initOpProperties(); }
}
