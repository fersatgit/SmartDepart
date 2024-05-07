<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:frmwrk="Corel Framework Data">
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="uiConfig/items">
    <xsl:copy>
    <xsl:apply-templates select="node()|@*"/>
    <itemData guid="SmartDepart" dynamicCommand="SmartDepart" dynamicCategory="ab489730-8791-45d2-a825-b78bbe0d6a5d" icon="guid://SmartDepart"/>    
    </xsl:copy>
  </xsl:template>  
  <xsl:template match="uiConfig/commandBars/commandBarData[@guid='74e03d83-404c-49f5-824a-fe0fd02ab29a']/toolbar/modeData/item[@guidRef='91774a85-4ed5-4eb5-9ba4-cb9190253446']"> //combine
    <xsl:copy-of select="."/>
    <item guidRef="SmartDepart"/>
  </xsl:template>  
  <xsl:template match="uiConfig/commandBars/commandBarData[@guid='74e03d83-404c-49f5-824a-fe0fd02ab29a']/toolbar/modeData/item[@guidRef='d5669070-c0f3-4fc0-b524-e33aeea7925f']"> //break apart
    <xsl:copy-of select="."/>
    <item guidRef="SmartDepart"/>
  </xsl:template>  
</xsl:stylesheet>