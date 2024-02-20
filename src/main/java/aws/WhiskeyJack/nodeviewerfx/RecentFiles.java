/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import java.nio.file.*;
import java.util.*;
import java.util.prefs.*;
import static java.util.prefs.Preferences.*;

public class RecentFiles {
    static final int saveLimit = 10;
    private static final String prefKey = "recentFiles";
    private final Map<Path, RecentFile> recents = new HashMap<>();
    Preferences prefs = userNodeForPackage(RecentFiles.class);
    public RecentFiles() {
        String v = prefs.get(prefKey, "");
        System.out.println("Restore file history: " + v);
        for(var key: v.split(";"))
            if(!key.isBlank()) {
                int divider = key.indexOf(':');
                if(divider > 0)
                    get(Path.of(key.substring(divider + 1))).rank
                        = Long.parseLong(key, 0, divider, 10);
            }
    }
    public final RecentFile get(Path p) {
        return recents.computeIfAbsent(p, p2 -> new RecentFile(p2));
    }
    public final RecentFile get(String p) {
        return get(Path.of(p));
    }
    public RecentFile[] sorted() {
        return recents.values().stream().filter(rf->!rf.getKey().startsWith("untitled"))
            .sorted((a, b) -> a.rank < b.rank ? -1 : a.rank > b.rank ? 1 : 0)
            .limit(saveLimit)
            .toArray(RecentFile[]::new);
    }
    public void save() {
        var sb = new StringBuilder();
        boolean first = true;
        for(var rf: sorted()) {
            if(first) first = false;
            else sb.append(';');
            rf.to(sb);
        }
        System.out.println("Save file history: " + sb);
        prefs.put(prefKey, sb.toString());
    }

    public class RecentFile {
        private final String key;
        private final Path path;
        private long rank;
        private RecentFile(long r, Path p) {
            path = p;
            key = path.getFileName().toString();
            rank = r;
        }
        private RecentFile(Path p) {
            this(0, p);
        }
        public RecentFile markUsed() {
            var newRank = System.currentTimeMillis();
            var oldRank = rank;
            rank = newRank;
            if(newRank > oldRank + 5) save();
            return this;
        }
        private RecentFile to(StringBuilder cs) {
            cs.append(rank).append(":").append(path);
            return this;
        }
        public String getKey() {
            return key;
        }
        public Path getPath() {
            return path;
        }
        @Override public String toString() { return path.toString(); }
    }
}
