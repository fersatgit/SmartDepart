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
    <itemData guid="SmartDepart" type="flyout" enable="*Or(*Bind(DataSource=WDocCommandsDS;Path=CanBreakApart),*Bind(DataSource=SelectionInfoDatasource;Path=IsValidCombine))" flyoutBarRef="SmartDepart_bar" icon="guid://SmartDepart_1"/>
    <itemData guid="SmartDepart1" type="button" userCaption="Умное деление" icon="guid://SmartDepart_1" onInvoke="*Bind(DataSource=SmartDepart;Path=0)"/>
    <itemData guid="SmartDepart2" type="button" userCaption="Резка струной" icon="guid://SmartDepart_2" onInvoke="*Bind(DataSource=SmartDepart;Path=1)"/>
    <itemData guid="SmartDepart3" type="button" userCaption="Триангуляция" icon="guid://SmartDepart_3" onInvoke="*Bind(DataSource=SmartDepart;Path=2)"/>
    <itemData guid="SmartDepart4" type="button" userCaption="Декомпозиция" icon="guid://SmartDepart_4" onInvoke="*Bind(DataSource=SmartDepart;Path=3)"/>        
    <itemData guid="SmartDepart5" type="statusText" userCaption="Погрешность линеаризации кривых" multiLine="true"/>
    <itemData guid="SmartDepart6" type="slider" value="*Bind(DataSource=SmartDepart;Path=4;BindType=TwoWay)" showEdit="true" rightLabelText="мм" numDecimalPlaces="2" rangeMin="0.01" rangeMax="10" increment="0.01"/>
    </xsl:copy>
  </xsl:template>  
  <xsl:template match="uiConfig/commandBars">
    <xsl:copy>
    <xsl:apply-templates select="node()|@*"/>    
    <commandBarData guid="SmartDepart_bar" type="toolbar" flyout="true">
	<menu>
    <item guidRef="SmartDepart1"/>    
    <item guidRef="SmartDepart2"/>
    <item guidRef="SmartDepart3"/>
    <item guidRef="SmartDepart4"/>
    <item guidRef="crlfrmwk_separator_horizontal"/>
    <item guidRef="SmartDepart5"/>
    <item guidRef="SmartDepart6"/>
    </menu>
	</commandBarData>
    </xsl:copy>
  </xsl:template>  
  <xsl:template match="uiConfig/commandBars/commandBarData[@guid='74e03d83-404c-49f5-824a-fe0fd02ab29a']/toolbar/modeData/item[@guidRef='91774a85-4ed5-4eb5-9ba4-cb9190253446']">
    <xsl:copy-of select="."/>
    <item guidRef="SmartDepart"/>  
   </xsl:template>  
  <xsl:template match="uiConfig/commandBars/commandBarData[@guid='74e03d83-404c-49f5-824a-fe0fd02ab29a']/toolbar/modeData/item[@guidRef='d5669070-c0f3-4fc0-b524-e33aeea7925f']">
    <xsl:copy-of select="."/>
    <item guidRef="SmartDepart"/>
  </xsl:template>  
</xsl:stylesheet>