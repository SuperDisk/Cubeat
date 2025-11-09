import tkinter as tk
from tkinter import ttk, messagebox

# Grid dimensions
COLS = 18
ROWS = 11
CELL = 32  # pixel size of each cell

# Tile states
BLANK = 0
WHITE = 1
BLACK = 2

STATE_TO_COLOR = {
    BLANK: "#2b2b2b",   # board background
    WHITE: "#ffffff",
    BLACK: "#000000",
}

STATE_TO_HEX = {
    BLANK: "00",
    WHITE: "80",
    BLACK: "81",
}

HEX_TO_STATE = {
    "00": BLANK,
    "80": WHITE,
    "81": BLACK,
}

class TileEditor(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("18x11 Tile Editor (White/Black)")

        # Model: rows x cols of states
        self.grid_state = [[BLANK for _ in range(COLS)] for __ in range(ROWS)]

        # UI
        self._build_ui()

        # Active tool: WHITE or BLACK or ERASER (mapped to BLANK)
        self.active_tool = WHITE

        # Keyboard shortcuts
        self.bind_all("<Key-w>", lambda e: self._set_tool(WHITE))
        self.bind_all("<Key-b>", lambda e: self._set_tool(BLACK))
        self.bind_all("<Key-e>", lambda e: self._set_tool(BLANK))

    def _build_ui(self):
        # Top controls
        controls = ttk.Frame(self, padding=8)
        controls.pack(side=tk.TOP, fill=tk.X)

        self.tool_var = tk.IntVar(value=WHITE)
        ttk.Label(controls, text="Tool:").pack(side=tk.LEFT, padx=(0,6))
        ttk.Radiobutton(controls, text="White (W)", variable=self.tool_var, value=WHITE,
                        command=lambda: self._set_tool(WHITE)).pack(side=tk.LEFT)
        ttk.Radiobutton(controls, text="Black (B)", variable=self.tool_var, value=BLACK,
                        command=lambda: self._set_tool(BLACK)).pack(side=tk.LEFT)
        ttk.Radiobutton(controls, text="Eraser (E)", variable=self.tool_var, value=BLANK,
                        command=lambda: self._set_tool(BLANK)).pack(side=tk.LEFT, padx=(0,12))

        ttk.Button(controls, text="Clear", command=self.clear_board).pack(side=tk.LEFT, padx=(0,8))
        ttk.Button(controls, text="Copy DB", command=self.copy_db_to_clipboard).pack(side=tk.LEFT, padx=(0,8))
        ttk.Button(controls, text="Load from Clipboard", command=self.load_from_clipboard).pack(side=tk.LEFT)

        # Canvas for grid
        w = COLS * CELL
        h = ROWS * CELL
        self.canvas = tk.Canvas(self, width=w, height=h, highlightthickness=0, bg="#1e1e1e")
        self.canvas.pack(side=tk.TOP, padx=8, pady=8)

        # Draw cells and grid lines
        self.cell_ids = [[None for _ in range(COLS)] for __ in range(ROWS)]
        for r in range(ROWS):
            for c in range(COLS):
                x0 = c * CELL
                y0 = r * CELL
                x1 = x0 + CELL
                y1 = y0 + CELL
                rect = self.canvas.create_rectangle(
                    x0, y0, x1, y1,
                    fill=STATE_TO_COLOR[BLANK],
                    outline="#3a3a3a", width=1
                )
                self.cell_ids[r][c] = rect

        # Mouse handlers
        self.canvas.bind("<Button-1>", self.on_left_click)    # place active tool
        self.canvas.bind("<B1-Motion>", self.on_left_click_drag)
        self.canvas.bind("<Button-3>", self.on_right_click)   # erase
        self.canvas.bind("<B3-Motion>", self.on_right_click_drag)

    def _set_tool(self, tool_state):
        self.active_tool = tool_state
        self.tool_var.set(tool_state)

    def _event_to_cell(self, event):
        c = event.x // CELL
        r = event.y // CELL
        if 0 <= r < ROWS and 0 <= c < COLS:
            return r, c
        return None

    def _apply_to_cell(self, r, c, state):
        if self.grid_state[r][c] != state:
            self.grid_state[r][c] = state
            self.canvas.itemconfig(self.cell_ids[r][c], fill=STATE_TO_COLOR[state])

    def on_left_click(self, event):
        pos = self._event_to_cell(event)
        if pos:
            r, c = pos
            self._apply_to_cell(r, c, self.active_tool)

    def on_left_click_drag(self, event):
        self.on_left_click(event)

    def on_right_click(self, event):
        pos = self._event_to_cell(event)
        if pos:
            r, c = pos
            self._apply_to_cell(r, c, BLANK)

    def on_right_click_drag(self, event):
        self.on_right_click(event)

    def clear_board(self):
        for r in range(ROWS):
            for c in range(COLS):
                self._apply_to_cell(r, c, BLANK)

    def copy_db_to_clipboard(self):
        """
        Formats the board as 11 lines, each:
        db $..,$.., ... (18 entries)
        where 00=blank, 80=white, 81=black
        """
        lines = []
        for r in range(ROWS):
            hexes = [f"${STATE_TO_HEX[self.grid_state[r][c]]}" for c in range(COLS)]
            lines.append("db " + ",".join(hexes))
        text = "\n".join(lines)

        self.clipboard_clear()
        self.clipboard_append(text)
        self.update()

    # --------- New: Load from Clipboard ----------
    def load_from_clipboard(self):
        try:
            text = self.clipboard_get()
        except tk.TclError:
            messagebox.showerror("Clipboard", "Clipboard is empty or not text.")
            return

        try:
            matrix = self._parse_db_text(text)
        except ValueError as e:
            messagebox.showerror("Parse Error", str(e))
            return

        # Apply to model + canvas
        for r in range(ROWS):
            for c in range(COLS):
                self._apply_to_cell(r, c, matrix[r][c])

    def _parse_db_text(self, text):
        """
        Expected format:
            11 lines like:
            db $00,$00,... (18 entries)
        Values allowed (case-insensitive): $00, $80, $81
        Returns: rows x cols list of states.
        Raises ValueError on any format issue.
        """
        # Normalize line endings, split, and keep non-empty lines
        lines = [ln.strip() for ln in text.replace("\r\n", "\n").replace("\r", "\n").split("\n")]
        lines = [ln for ln in lines if ln != ""]
        if len(lines) != ROWS:
            raise ValueError(f"Expected {ROWS} rows, found {len(lines)}.")

        parsed = []
        for row_idx, raw in enumerate(lines):
            # allow leading 'db' (case-insensitive) and optional spaces
            if not raw.lower().startswith("db"):
                raise ValueError(f"Row {row_idx+1}: must start with 'db'.")
            content = raw[2:].strip()  # remove leading 'db'
            if content.startswith(" "):
                content = content.strip()

            # Remove any trailing comments after ';' if present
            if ";" in content:
                content = content.split(";", 1)[0].strip()

            # Split by commas
            parts = [p.strip() for p in content.split(",") if p.strip() != ""]
            if len(parts) != COLS:
                raise ValueError(f"Row {row_idx+1}: expected {COLS} entries, found {len(parts)}.")

            row_states = []
            for col_idx, token in enumerate(parts):
                # Accept tokens like $00 / $80 / $81 (case-insensitive)
                if not token.startswith("$") or len(token) != 3:
                    raise ValueError(f"Row {row_idx+1}, Col {col_idx+1}: invalid token '{token}'.")
                code = token[1:].upper()
                if code not in HEX_TO_STATE:
                    raise ValueError(f"Row {row_idx+1}, Col {col_idx+1}: unsupported value '${code}' (use $00, $80, $81).")
                row_states.append(HEX_TO_STATE[code])

            parsed.append(row_states)

        return parsed
    # ---------------------------------------------

if __name__ == "__main__":
    app = TileEditor()
    app.mainloop()
