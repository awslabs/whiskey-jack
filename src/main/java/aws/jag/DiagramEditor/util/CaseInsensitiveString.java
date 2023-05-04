/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.util;

public class CaseInsensitiveString implements CharSequence {
    private final String value;
    private final int hash;

    public CaseInsensitiveString(String v) {
        value = v;
        hash = v.toLowerCase().hashCode();
    }

    @Override
    public boolean equals(Object o) {
        return this == o ||
                o instanceof CaseInsensitiveString v
                && value.equalsIgnoreCase(v.value);
    }

    @Override
    public int hashCode() {
        return hash;
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public int length() {
        return value.length();
    }

    @Override
    public char charAt(int index) {
        return value.charAt(index);
    }

    @Override
    public CharSequence subSequence(int start, int end) {
        return value.subSequence(start, end);
    }
}
