/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.DiagramEditor.nodegraph;

import java.util.*;

public class ErrorCode {
    private final String name;
    @SuppressWarnings("LeakingThisInConstructor")
    private ErrorCode(String n) {
        name = n;
    }
    private static final Map<String, ErrorCode> errorCodes = new HashMap<>();
    public static final ErrorCode of(String name) {
        return errorCodes.computeIfAbsent(name, n->new ErrorCode(n));
    }
    public static final ErrorCode allIsWell = of("allIsWell");
    public static final ErrorCode typeMismatch = of("typeMismatch");
    public static final ErrorCode valueExpected = of("valueExpected");
    public static final ErrorCode crossDomain = of("crossDomain");
    @Override
    public String toString() {
        return name;
    }
}
