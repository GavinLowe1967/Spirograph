DIR = spirograph

CP = .:scala-swing_2.13-3.0.0.jar

all: $(DIR)/Spirograph.class

$(DIR)/Wheel.class: $(DIR)/Piece.class

$(DIR)/Model.class:  $(DIR)/Wheel.class

# ===== Controller

$(DIR)/Controller.class:  $(DIR)/Model.class $(DIR)/FrameT.class

# ===== View

$(DIR)/DrawingPanel.class: $(DIR)/Model.class $(DIR)/Controller.class

$(DIR)/SpirographMainFrame.class: $(DIR)/DrawingPanel.class $(DIR)/FrameT.class


$(DIR)/Spirograph.class: $(DIR)/SpirographMainFrame.class $(DIR)/Controller.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

$(DIR)/%.class:     %.scala
	fsc -deprecation -cp $(CP) $<
