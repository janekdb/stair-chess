package chess.model

trait ConfigurationChangedSubscriber {

  def onConfigurationChanged(configuration: Configuration)
}