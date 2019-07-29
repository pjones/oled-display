#include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>
#include <string.h>

/******************************************************************************/
Adafruit_SSD1306 display(128, 32, &Wire, -1);

/******************************************************************************/
// Current state:
char current_text[256] = "";
uint8_t current_text_size = 2;
uint32_t current_flash    = 0;
uint32_t next_display_change = 0;
bool current_display_mode = true;
bool text_changed = false;
String command = String();

/******************************************************************************/
// Draw the current text buffer to the screen.
void draw() {
  display.clearDisplay();
  display.setTextSize(current_text_size);
  display.setTextColor(WHITE);
  display.setCursor(0, 0);
  display.cp437(true);

  for (uint8_t i=0; i<sizeof(current_text); ++i) {
    if (current_text[i] == '\0') break;
    display.write(current_text[i]);
  }

  display.display();
}

/******************************************************************************/
// Clear the text buffer and the screen.
void clear() {
  current_text[0] = '\0';
  display.clearDisplay();
  display.display();
}

/******************************************************************************/
// Reset all state.
void reset() {
  clear();
  current_text_size = 2;
  current_flash = 0;
  next_display_change = 0;
  current_display_mode = true;
  text_changed = false;
}

/******************************************************************************/
void parse_command() {
  char command_code;
  bool result = true;

  if (command.length() == 1) {
    command_code = command.charAt(0);
    command = F("");
  } else if (command.length() >= 1) {
    command_code = command.charAt(0);
    command = command.substring(1);
  } else return;

  switch (command_code) {
  case 'C': /* Clear the display */
    clear();
    break;

  case 'F': /* Flash the display */
    {
      uint32_t previous = current_flash;
      current_flash = command.toInt();

      if (current_flash != previous && current_flash > 0) {
        current_display_mode = false;
        next_display_change = millis() + current_flash;
      } else if (current_flash == 0) {
        current_display_mode = true;
        next_display_change = 0;
      }
    }
    break;

  case 'R': /* Reset all options */
    reset();
    break;

  case 'S': /* Set text size */
    {
      int size = command.toInt();

      if (size > 0 && size <= 4)
        current_text_size = size;
    }
    break;

  case 'T': /* Text to diplay */
    {
      size_t n = min(sizeof(current_text) - 1, command.length());
      strncpy(current_text, command.c_str(), n);
      current_text[n] = '\0';
    }
    break;

  default:
    result = false;
    break;
  }

  if (result) {
    text_changed = true;
    Serial.println(F("ACK"));
  } else {
    Serial.print(F("ERR"));
  }

  Serial.flush();
  command = F("");
}

/******************************************************************************/
void setup() {
  // Disable the TX/RX LEDs:
  pinMode(LED_BUILTIN_TX,INPUT);
  pinMode(LED_BUILTIN_RX,INPUT);

  Serial.begin(9600);

  if(!display.begin(SSD1306_SWITCHCAPVCC, 0x3C)) {
    for(;;) Serial.println(F("SSD1306 allocation failed"));
  }

  while (!Serial);
}

/******************************************************************************/
void loop() {
  if (Serial.available() > 0) {
    char c = Serial.read();
    if (c == '\r') parse_command();
    else if (c != 0 && c != '\n') command += c;
  }

  if (current_flash > 0) {
    if (millis() >= next_display_change) {
      current_display_mode = !current_display_mode;
      next_display_change = millis() + current_flash;
    }
  }

  if (current_display_mode) {
    if (text_changed) {
      draw();
      text_changed = false;
    }
  } else {
    display.clearDisplay();
    display.display();
    text_changed = true;
  }
}
