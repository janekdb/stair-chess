package chess.model

trait ConfigurationChangedSubscriber {

  def onConfigurationChanged(configuration: ConfigurationView): Unit
}
