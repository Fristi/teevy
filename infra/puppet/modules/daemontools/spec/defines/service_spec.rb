require 'spec_helper'

describe 'daemontools::service' do
  context 'default logpath' do
    let(:title) {'myapp'}
    let(:params) {{
      :command => '/usr/bin/myapp'
    }}

    it { should contain_file('/etc/myapp') }
    it { should contain_file('/etc/myapp/run') }
    it { should contain_file('/etc/myapp/log') }
    it { should contain_file('/etc/myapp/log/run') }
    it { should contain_file('/var/log/myapp') }
    it { should contain_service('myapp') }
  end

  context 'custom logpath' do
    let(:title) {'myapp'}
    let(:params) {{
      :command => '/usr/bin/myapp',
      :logpath => '/some/log/dir'
    }}

    it { should contain_file('/etc/myapp') }
    it { should contain_file('/etc/myapp/run') }
    it { should contain_file('/etc/myapp/log') }
    it { should contain_file('/etc/myapp/log/run') }
    it { should contain_file('/some/log/dir') }
    it { should contain_service('myapp') }
  end
end
