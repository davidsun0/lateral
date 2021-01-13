package lateral.interactive;

import java.io.IOException;
import java.nio.file.*;

import static java.nio.file.StandardWatchEventKinds.*;

public class FileWatcher {
    WatchService watchService;
    WatchKey key;

    FileWatcher(Path dir) throws IOException {
        watchService = FileSystems.getDefault().newWatchService();
        key = dir.register(watchService, ENTRY_MODIFY, ENTRY_CREATE);
    }

    void processEvents() {
        while(true) {
            WatchKey key;
            try {
                key = watchService.take();
            } catch (InterruptedException e) {
                return;
            }
            for(WatchEvent<?> event : key.pollEvents()) {
                WatchEvent.Kind<?> kind = event.kind();
                if(kind == OVERFLOW)
                    continue;
                Path name = ((WatchEvent<Path>)event).context();
                System.out.printf("%s: %s\n", event.kind().name(), name);
            }
            key.reset();
        }
    }

    public static void main(String[] args) throws IOException{
        new FileWatcher(Paths.get("src/lisp/")).processEvents();
    }
}
