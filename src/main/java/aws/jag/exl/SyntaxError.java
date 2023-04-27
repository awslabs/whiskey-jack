/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.exl;

import java.io.*;


public class SyntaxError extends IOException {
    final int line, col;
    public SyntaxError(String msg, Tokenizer where) {
        super(where.line+"@"+where.col+": "+msg);
        line = where.line;
        col = where.col;
    }
}
