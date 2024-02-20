/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.nodegraph;

import java.util.*;

public class ErrorCode {
    private final String name;
    private final boolean warning;
    @SuppressWarnings("LeakingThisInConstructor")
    private ErrorCode(String n, boolean w) {
        name = n;
        warning = w;
    }
    private static final Map<String, ErrorCode> errorCodes = new HashMap<>();
    public static final ErrorCode error(String name) {
        return errorCodes.computeIfAbsent(name, n->new ErrorCode(n, false));
    }
    public static final ErrorCode warning(String name) {
        return errorCodes.computeIfAbsent(name, n->new ErrorCode(n, true));
    }
    public boolean isWarning() { return warning; }
    public static final ErrorCode allIsWell = error("allIsWell");
    public static final ErrorCode typeMismatch = error("typeMismatch");
    public static final ErrorCode valueExpected = error("valueExpected");
    public static final ErrorCode crossDomain = error("crossDomain");
    public static final ErrorCode highBandwidth = warning("highBandwidth");
    public static final ErrorCode highCompute = warning("highCompute");
    @Override
    public String toString() {
        return name;
    }
}
