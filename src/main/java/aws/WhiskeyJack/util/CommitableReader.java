/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.util;

import static aws.WhiskeyJack.util.CommitableFile.*;
import java.io.*;
import static java.lang.System.*;
import static java.nio.file.Files.*;
import java.nio.file.*;
import static java.nio.file.Files.*;

public final class CommitableReader {

    private final Path target;
    private final Path backup;

    private CommitableReader(Path target, Path backup) {
        this.target = target;
        this.backup = backup;
    }

    public static CommitableReader of(Path path) {
        return new CommitableReader(path, getBackupFile(path));
    }

    /**
     * Read and validate the content of the given CommitableFile.
     *
     * @param validator CrashableFunction to read and validate file content
     * @throws IOException on I/O error
     */
    public void read(CrashableFunction<InputStream, Void, IOException> validator) throws IOException {
        try (var t = newInputStream(target)) {
            validator.apply(t);
        } catch (IOException e1) {
            if (!exists(backup)) {
                throw e1;
            }
            try (var b = newInputStream(backup)) {
                validator.apply(b);
            } catch (IOException e2) {
                e1.addSuppressed(e2);
                throw e1;
            }

            move(backup, target);
            out.println("Revert file to backup version: "+target);
        }
    }
}
