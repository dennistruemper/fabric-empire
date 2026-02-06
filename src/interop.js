// Fiber Empire — JavaScript interop for localStorage save/load

const SAVE_KEY = 'fiber-empire-save';

// Called BEFORE Elm app starts. Return initial flags.
export const flags = ({ env }) => {
  return {};
};

// Called AFTER Elm app starts. Set up port subscriptions.
export const onReady = ({ app, env }) => {
  // Save game: listen for outgoing save requests
  if (app.ports && app.ports.saveGame) {
    app.ports.saveGame.subscribe((data) => {
      try {
        localStorage.setItem(SAVE_KEY, JSON.stringify(data));
      } catch (e) {
        console.warn('Failed to save game:', e);
      }
    });
  }

  // Load game: send saved state to Elm via incoming port
  if (app.ports && app.ports.loadedGame) {
    try {
      const saved = localStorage.getItem(SAVE_KEY);
      if (saved) {
        const parsed = JSON.parse(saved);
        // Small delay to ensure Elm subscriptions are ready
        requestAnimationFrame(() => {
          app.ports.loadedGame.send(parsed);
        });
      }
    } catch (e) {
      console.warn('Failed to load saved game:', e);
    }
  }
};
